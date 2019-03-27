{-# LANGUAGE DeriveGeneric #-}

module Config (getKey) where

import           Data.Yaml
import           GHC.Generics

newtype Key = Key { key :: String }
  deriving (Show, Generic)

instance FromJSON Key

getKey :: IO String
getKey = key <$> readYaml

readYaml :: IO Key
readYaml = do
  f <- decodeFileEither "./config.yaml"
  case f of
    Left e  -> (error . show) e
    Right k -> return k