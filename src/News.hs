{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module News (getTypingStrings) where

import           Config
import           Data.Aeson
import           GHC.Generics
import           Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as B8

newtype Response = Response { articles :: [Article] }
  deriving (Show, Generic, FromJSON)

newtype Article = Article { description :: Maybe String }
  deriving (Show, Generic, FromJSON)

getTypingStrings :: IO [String]
getTypingStrings = filter (/= "") . map (getDescString . description)
  <$> getArticlesFromApi

getArticlesFromApi :: IO [Article]
getArticlesFromApi = do
  apiKey <- getKey
  res <- do
    req <- setQueryString
          [ (B8.pack "country", Just (B8.pack "us"))
          , (B8.pack "pageSize", Just (B8.pack "100"))
          , (B8.pack "apiKey", Just (B8.pack apiKey))]
           <$> parseUrl "https://newsapi.org/v2/top-headlines"
    man <- newManager tlsManagerSettings
    httpLbs req man
  case eitherDecode $ responseBody res of
    Left er  -> error er
    Right dc -> return $ articles dc

getDescString :: Maybe String -> String
getDescString Nothing  = ""
getDescString (Just x) = toInputable x
  where
    toInputable = filter (`elem` [' ' .. '~'])
