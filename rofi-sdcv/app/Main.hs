#!/usr/bin/env cabal
{- cabal:
build-depends:
    , aeson
    , base        >=4.16
    , bytestring
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

import           Control.Exception        (SomeException, catch)
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
import           System.IO                (readFile')
import           System.Process           (callProcess, readProcessWithExitCode)
import qualified Text.HTML.Parser         as HTML
import qualified Text.HTML.Tree           as HTML
import qualified Text.LineBreak           as LineBreak

-- rofi script
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

-- sdcv
data SdcvResult = SdcvResult
  { sdcvResultDict       :: !Text
  , sdcvResultWord       :: !Text
  , sdcvResultDefinition :: !Text
  }
  deriving (Show, Eq, Generic)

standardAesonGenericOptions :: [(String, String)] -> Aeson.Options
standardAesonGenericOptions fieldLabels =
  Aeson.defaultOptions
  { Aeson.omitNothingFields  = True -- ^ Omit 'Nothing' fields rather than converting them to 'null'.
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

-- TODO: Handle tags like <br> and <ol>. Ref: tool `html2text`
renderHtml :: [HTML.Token] -> String
renderHtml tokens = case HTML.tokensToForest tokens of
  Left err     -> "<error> " <> show err
  Right _forest ->
    let textTokens = L.filter isTextToken tokens
     in TL.unpack (HTML.renderTokens $ L.intersperse (HTML.ContentText "\n") textTokens)
  where
    isTextToken :: HTML.Token -> Bool
    isTextToken (HTML.ContentText _) = True
    isTextToken _                    = False

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

dictWordSeparator :: String
dictWordSeparator = " :: "

renderSdcvResults :: [SdcvResult] -> String
renderSdcvResults xs =
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

-- TODO: Configurable GUI attrs
main :: IO ()
main = lookupEnv "ROFI_RETV" >>= \case
  Nothing  -> do
    myPath <- getExecutablePath
    callProcess "rofi" ["-show", "custom"
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
      (dict_m, word) <- do
        (_, _, arg1WithoutMarkup) <- Pango.parseMarkup (args !! 0) '\0' -- FIXME: handle GError
        -- Note: restrict results to certain dict
        case L.splitOn dictWordSeparator arg1WithoutMarkup of
          d:w:_ -> pure (Just d, w)
          _     -> pure (Nothing, arg1WithoutMarkup)
      sdcvResults <- lookupWordFromSdcv dict_m word
      putStr $ renderSdcvResults sdcvResults
