{-# LANGUAGE DeriveGeneric #-}

module Config (getKey, isFixConsoleWidth) where

import           Data.List
import           Data.Yaml
import           GHC.Generics
import           System.Environment

data Key = Key { key :: String, fixConsoleWidth :: Bool }
  deriving (Show, Generic)

instance FromJSON Key

getKey :: IO String
getKey = key <$> readYaml

isFixConsoleWidth :: IO Bool
isFixConsoleWidth = fixConsoleWidth <$> readYaml

readYaml :: IO Key
readYaml = do
  d <- dropWhileEnd (/= '/') <$> getExecutablePath
  f <- decodeFileEither $ d ++ "config.yaml"
  case f of
    Left e  -> (error . show) e
    Right c -> return c