{-# LANGUAGE DeriveGeneric #-}

module Config (getKey) where

import           Data.Yaml
import           Data.List
import           GHC.Generics
import           System.Environment

newtype Key = Key { key :: String }
  deriving (Show, Generic)

instance FromJSON Key

getKey :: IO String
getKey = key <$> readYaml

readYaml :: IO Key
readYaml = do
  d <- dropWhileEnd (/= '/') <$> getExecutablePath
  f <- decodeFileEither $ d ++ "config.yaml"
  case f of
    Left e  -> (error . show) e
    Right k -> return k