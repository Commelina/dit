module Misstro
  ( distrustAll
  ) where

import           Control.Monad     (join)
import qualified Data.List         as L
import           Data.List.Split   (splitOn)
import           Data.Maybe        (catMaybes)
import qualified Safe              as Safe
import           System.Exit       (ExitCode (..))
import           System.Process    (readProcessWithExitCode)

import           Misstro.Blocklist (blocklistExact, blocklistMatch)

data Cert = Cert
  { certId       :: !String
  , certType     :: !String
  , certLabel    :: !String
  , certTrust    :: !String
  , certCategory :: !String
  } deriving (Eq, Show)

parseCert :: String -> Maybe Cert
parseCert s =
  let id_m    = Safe.headMay ls
      type_m  = join $ L.stripPrefix "    type: "     <$> (ls `Safe.atMay` 1)
      label_m = join $ L.stripPrefix "    label: "    <$> (ls `Safe.atMay` 2)
      trust_m = join $ L.stripPrefix "    trust: "    <$> (ls `Safe.atMay` 3)
      cat_m   = join $ L.stripPrefix "    category: " <$> (ls `Safe.atMay` 4)
   in Cert
        <$> id_m
        <*> type_m
        <*> label_m
        <*> trust_m
        <*> cat_m
  where
    ls = L.lines s

listAllCerts :: IO [Cert]
listAllCerts = do
  (code, out, _) <- readProcessWithExitCode
                    "/usr/bin/trust"
                    ["list", "--filter=certificates"]
                    ""
  case code of
    ExitFailure _ -> do
      putStrLn "!! Failed to list certificates, do you have /usr/bin/trust installed?"
      return []
    ExitSuccess   -> return $ catMaybes $ parseCert <$> splitOn "\n\n" out

distrustCert :: Cert -> IO ()
distrustCert cert = case certTrust cert of
  "distrusted" -> putStrLn $ ".. Cert " <> certLabel cert <> " already distrusted, skipping."
  _            -> do
    let fileName = L.map (\c -> if c == ' ' then '_' else c) (certLabel cert) <> ".pem"
    (code, _, _) <- readProcessWithExitCode
                    "/usr/bin/trust"
                    [ "extract"
                    , "--format=pem-bundle"
                    , "--filter=" <> certId cert
                    , "/etc/ca-certificates/trust-source/blocklist/" <> fileName
                    ]
                    ""
    case code of
      ExitFailure _ -> putStrLn $ "!! Failed to distrust cert: " <> certLabel cert <> ". " <>
                                  "Did you run this as root? " <>
                                  "Or do you have /usr/bin/trust installed?"
      ExitSuccess   -> putStrLn $ "-> Distrusted cert: " <> certLabel cert

updateCATrust :: IO ()
updateCATrust = do
  putStrLn "Updating CA trust..."
  (code, _, _) <- readProcessWithExitCode "/usr/bin/update-ca-trust" [] ""
  case code of
    ExitFailure _ -> putStrLn $ "!! Failed to update CA trust. " <>
                                "Did you run this as root? " <>
                                "Or do you have /usr/bin/update-ca-trust installed?"
    ExitSuccess   -> putStrLn "-> CA trust updated successfully."

distrustAll :: IO ()
distrustAll = do
  certs <- listAllCerts
  let exactList = L.filter (\c -> certLabel c `elem` blocklistExact) certs
      matchList = L.filter (\c -> any (`L.isInfixOf` certLabel c) blocklistMatch) certs
      _fullList = L.nub (exactList <> matchList)
  putStrLn $ "============ Exact Matches ============ " <> "total=" <> show (length exactList)
  mapM_ (putStrLn . certLabel) exactList
  putStrLn $ "=========== Keyword Matches =========== " <> "total=" <> show (length matchList)
  mapM_ (putStrLn . certLabel) matchList
  putStrLn "========================================="
  putStrLn $ "Distrusting " <> show (length exactList) <> " exact matches..."
  mapM_ distrustCert $ exactList
  updateCATrust
