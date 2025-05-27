#!/usr/bin/env cabal
-- Note: cabal script does not support `project: packages`, see #8024.
--       Find and run the compiled binary instead.
{- cabal:
build-depends:
    , aeson
    , aki
    , base        >=4.16
    , bytestring
    , glib
    , html-parse
    , linebreak
    , pango
    , process
    , split
    , text
-}

{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception        (SomeException, catch, try)
import           Data.Aeson               (FromJSON (..), ToJSON (..),
                                           genericParseJSON, genericToJSON)
import qualified Data.Aeson               as Aeson
import qualified Data.List                as L
import qualified Data.List.Split          as L
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           GHC.Generics             (Generic)
import qualified Graphics.Rendering.Pango as Pango
import           System.Environment       (getArgs, getExecutablePath,
                                           lookupEnv)
import           System.Exit              (ExitCode (..), exitSuccess)
import qualified System.Glib.GError       as G
import           System.IO                (readFile')
import           System.Process           (callProcess, readProcessWithExitCode)
import qualified Text.HTML.Parser         as HTML
import qualified Text.HTML.Tree           as HTML
import qualified Text.LineBreak           as LineBreak

import qualified Aki                      as Aki

--------------------------------------------------------------------------------
-- rofi script
--------------------------------------------------------------------------------

data RofiModeOption
  = RofiModeOptionPrompt           !String
  | RofiModeOptionMessage          !String
  | RofiModeOptionEnableMarkupRows !Bool
  | RofiModeOptionData             !String
  deriving (Show)

renderRofiModeOptions :: [RofiModeOption] -> String
renderRofiModeOptions [] = ""
renderRofiModeOptions xs = L.concat (L.map render xs)
  where
    render :: RofiModeOption -> String
    render (RofiModeOptionPrompt           p) = ['\0'] <> "prompt"      <> ['\x1f'] <> p                               <> ['\n']
    render (RofiModeOptionMessage          m) = ['\0'] <> "message"     <> ['\x1f'] <> m                               <> ['\n']
    render (RofiModeOptionEnableMarkupRows b) = ['\0'] <> "markup-rows" <> ['\x1f'] <> (if b then "true" else "false") <> ['\n']
    render (RofiModeOptionData             v) = ['\0'] <> "data"        <> ['\x1f'] <> v                               <> ['\n']

--------------------------------------------------------------------------------
-- sdcv
--------------------------------------------------------------------------------

data SdcvResult = SdcvResult
  { sdcvResultDict       :: !Text
  , sdcvResultWord       :: !Text
  , sdcvResultDefinition :: !Text
  }
  deriving (Show, Eq, Generic)

standardAesonGenericOptions :: [(String, String)] -> Aeson.Options
standardAesonGenericOptions fieldLabels =
  Aeson.defaultOptions
  { Aeson.omitNothingFields  = True
  , Aeson.fieldLabelModifier = \field ->
      fromMaybe ("JSON field missing: " <> show field) (L.lookup field fieldLabels)
  }

sdcvResultAesonOptions :: Aeson.Options
sdcvResultAesonOptions = standardAesonGenericOptions
  [ ("sdcvResultDict", "dict")
  , ("sdcvResultWord", "word")
  , ("sdcvResultDefinition", "definition")
  ]

instance FromJSON SdcvResult where
  parseJSON = genericParseJSON sdcvResultAesonOptions
instance ToJSON SdcvResult where
  toJSON = genericToJSON sdcvResultAesonOptions

lookupWordFromSdcv :: Maybe String -> String -> IO [SdcvResult]
lookupWordFromSdcv dict_m s = do
  let dictArgs = case dict_m of
        Nothing   -> []
        Just dict -> ["-u", dict]
  (exitCode, out, _err) <- readProcessWithExitCode "sdcv" (["-0", "-n", "-j"] <> dictArgs <> [s]) ""
  case exitCode of
    ExitFailure _ -> exitSuccess
    ExitSuccess   -> case Aeson.eitherDecode' (TL.encodeUtf8 $ TL.pack out) of
      Right (x :: [SdcvResult]) -> return x
      Left _err                 -> return []

--------------------------------------------------------------------------------
-- rendering
--------------------------------------------------------------------------------

dictWordSeparator :: String
dictWordSeparator = " :: "

-- TODO: Handle tags like <br> and <ol>. Ref: tool `html2text`
renderHtml :: [HTML.Token] -> String
renderHtml tokens = case HTML.tokensToForest tokens of
  Left err      -> "<error> " <> show err
  Right _forest ->
    let textTokens = L.filter isTextToken tokens
     in TL.unpack (HTML.renderTokens $ L.intersperse (HTML.ContentText "\n") textTokens)
  where
    isTextToken :: HTML.Token -> Bool
    isTextToken (HTML.ContentText _) = True
    isTextToken _                    = False

renderToRofi :: [SdcvResult] -> String
renderToRofi xs =
  let rofiModeOptionsString = renderRofiModeOptions rofiModeOptions
   in rofiModeOptionsString <>
      L.intercalate "\n" (renderSdcvResult <$> xs)
  where
    rofiModeOptions = [ RofiModeOptionEnableMarkupRows True
                      ]

    renderPotentialHtml :: Text -> String
    renderPotentialHtml t =
      let tokens = HTML.canonicalizeTokens (HTML.parseTokens t)
       in renderHtml tokens

    renderSdcvResult :: SdcvResult -> String
    renderSdcvResult SdcvResult{..} =
      let titleString = Pango.markSpan titleAttrs (T.unpack sdcvResultDict)
                     <> dictWordSeparator
                     <> T.unpack sdcvResultWord
          defString   = LineBreak.breakString lineBreakFormat (renderPotentialHtml sdcvResultDefinition)
       in titleString <> ['\n'] <> defString
      where
        titleAttrs = [ Pango.FontSize       Pango.SizeMedium
                     , Pango.FontWeight     Pango.WeightBold
                     , Pango.FontForeground "#2196f3"
                     ]
        lineBreakFormat = LineBreak.BreakFormat
                          { bfMaxCol       = 120
                          , bfTabRep       = 2
                          , bfHyphenSymbol = '-'
                          , bfHyphenator   = Just LineBreak.english_US
                          }

renderToAnki :: Text -> [SdcvResult] -> (Text, Text)
renderToAnki word xs =
  let front = withWordStyle word
      back  = T.intercalate newline (renderDef <$> xs)
   in (front, back)
  where
    newline :: Text
    newline = "<br>"

    withWordStyle, withDefStyle :: Text -> Text
    withWordStyle s = "<div style=\"text-align: center; font-size: 200%\">" <> s <> "</div>"
    withDefStyle  s = "<div style=\"text-align: left;\">"                   <> s <> "</div>"

    renderDef :: SdcvResult -> Text
    renderDef SdcvResult{..} =
      sdcvResultDict <> (T.pack dictWordSeparator)  <> sdcvResultWord <>
      newline                                                         <>
      withDefStyle (T.replace "\n" newline sdcvResultDefinition)

-- <titleLine> -> Either <err> (<dict_m>, <word>)
-- Note: For restricting results to certain dict.
--       It returns `(Nothing, word)` for the first lookup,
--       and `(Just dict, word)` when selecting a dict from the results.
parseRofiTitle :: String -> IO (Either String (Maybe String, String))
parseRofiTitle s = do
  try (Pango.parseMarkup s '\0') >>= \case
    Left  (e :: G.GError)           -> pure (Left $ "Error parsing markup: " <> show e)
    Right (_, _, arg1WithoutMarkup) -> do
      case L.splitOn dictWordSeparator arg1WithoutMarkup of
        d:w:_ -> pure . Right $ (Just d, w)
        _     -> pure . Right $ (Nothing, arg1WithoutMarkup)

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

-- TODO: Configurable GUI attrs
main :: IO ()
main = lookupEnv "ROFI_RETV" >>= \case
  Nothing  -> do
    myPath <- getExecutablePath
    callProcess "rofi" [ "-show", "custom"
                       , "-modi", "custom:" <> myPath
                       , "-async"
                       , "-dpi", "144"
                       , "-theme-str", "window {width: 122ch;} listview {lines: 30; scrollbar-width: 2ch;}"
                       ]
  Just "0" -> do
    let rofiModeOptionsString = renderRofiModeOptions initRofiModeOptions
    allWords <- readAllWords
    putStr $ rofiModeOptionsString <> unlines allWords
  Just "1" -> performLookup
  Just "2" -> performLookup
  Just _   -> return ()
  where
    readAllWords :: IO [String]
    readAllWords = do
      enPath <- fromMaybe "/home/commelina/Code/dit/rofi-sdcv/local/en.txt" <$> lookupEnv "DIT_WORDLIST_EN"
      svPath <- fromMaybe "/home/commelina/Code/dit/rofi-sdcv/local/sv.txt" <$> lookupEnv "DIT_WORDLIST_SV"
      en <- readFile' enPath `catch` \(_ :: SomeException) -> return "Note: set DIT_WORDLIST_EN for English wordlist"
      sv <- readFile' svPath `catch` \(_ :: SomeException) -> return "Note: set DIT_WORDLIST_SV for Swedish wordlist"
      return $ L.lines en <> L.lines sv

    initRofiModeOptions :: [RofiModeOption]
    initRofiModeOptions = [ RofiModeOptionPrompt "sdcv"
                          ]

    performLookup :: IO ()
    performLookup = do
      args <- getArgs
      parseRofiTitle (args !! 0) >>= \case
        Left _               -> exitSuccess
        Right (dict_m, word) -> do
          sdcvResults <- lookupWordFromSdcv dict_m word
          -- Note: add to Anki only when a dictionary is selected
          case dict_m of
            Nothing    -> pure ()
            Just _dict -> uncurry Aki.addBasicNote (renderToAnki (T.pack word) sdcvResults) >>= \case
              Left err -> putStrLn $ "[Anki] "                      <> err
              Right _  -> putStrLn $ "[Anki] Added note for word: " <> word
          -- Note: always render to rofi
          putStr $ renderToRofi sdcvResults
