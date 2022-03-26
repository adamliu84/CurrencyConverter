{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (forConcurrently)
import Control.Lens ((^.))
import Data.Aeson (FromJSON, Object, Value (..), parseJSON)
import Data.Aeson.Key (toString)
import Data.Aeson.KeyMap (toList)
import Data.Aeson.Types (Parser)
import Data.Scientific (toRealFloat)
import Data.Text (Text, unpack)
import Network.Wreq (asJSON, get, responseBody)

apiKey :: Text
apiKey = "xyz_currconv_api_key"

baseCurrency :: Text
baseCurrency = "USD"

quotes :: [Text]
quotes = ["SGD", "MYR", "GBP", "CAD", "EUR", "PHP", "MYR"]

data CurrencyRate = CurrencyRate
  { base :: String,
    quote :: String,
    value :: Float
  }
  deriving (Show)

-- https://stackoverflow.com/questions/42578331/aeson-parse-json-with-unknown-key-in-haskell
instance FromJSON CurrencyRate where
  parseJSON val = parseEntry val :: Parser CurrencyRate
    where
      parseEntry :: Value -> Parser CurrencyRate
      parseEntry (Data.Aeson.Object o) = pure $ CurrencyRate base quote value
        where
          kv = (toList o !! 0)
          bq = toString $ fst kv
          base = take 3 bq
          quote = drop 4 bq
          value = case (snd kv) of
            Number v -> toRealFloat v
            _ -> error "Unable to convert value"

main :: IO ()
main = do
  forConcurrently quotes (\quote -> getQuote baseCurrency quote) >>= mapM_ print
  return ()

getQuote :: Text -> Text -> IO CurrencyRate
getQuote b q = do
  res <- asJSON =<< get api_url
  pure (res ^. responseBody)
  where
    api_url :: String
    api_url = unpack $ "https://free.currconv.com/api/v7/convert?q=" <> b <> "_" <> q <> "&compact=ultra&apiKey=" <> apiKey