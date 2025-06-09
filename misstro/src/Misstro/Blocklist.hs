module Misstro.Blocklist
  ( blocklistExact
  , blocklistMatch
  ) where

blocklistExact :: [String]
blocklistExact =
  [ "BJCA Global Root CA1"
  , "BJCA Global Root CA2"
  , "CFCA EV ROOT"
  , "GDCA TrustAUTH R5 ROOT"
  , "Hongkong Post Root CA 3"
  -- iTrus 天威诚信
  , "vTrus ECC Root CA"
  , "vTrus Root CA"
  -- Shanghai CA
  , "UCA Extended Validation Root"
  , "UCA Global G2 Root"
  -- 亚洲诚信
  , "TrustAsia Global Root CA G3"
  , "TrustAsia Global Root CA G4"
  -- Chunghwa Telecom
  , "HiPKI Root CA - G1"
  , "ePKI Root Certification Authority"
  ]

blocklistMatch :: [String]
blocklistMatch =
  [ "China"
  , "CHINA"
  , "BJCA"
  , "BEIJING"
  , "Beijing"
  , "CFCA"
  , "GDCA"
  , "Hongkong"
  , "Hong Kong"
  , "HONGKONG"
  , "vTrus"
  , "iTrus"
  , "UCA"
  , "SHANGHAI"
  , "Shanghai"
  , "TrustAsia"
  , "Chunghwa Telecom"
  , "HiPKI"
  , "ePKI"
  ]
