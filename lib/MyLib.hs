{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Data.Aeson
import Data.Aeson.Optics
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as T
import Data.Foldable (Foldable (..))
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Optics

data PathKey
  = Required Key
  | Optional Key
  | Repeated Key

shred :: Value -> [PathKey] -> [(Int, Int, Maybe Value)]
shred (Array xs) col = toList xs >>= \x -> shredGo 0 0 x col []
shred x col = shredGo 0 0 x col []

shredGo ::
  Int ->
  Int ->
  Value ->
  [PathKey] ->
  [(Int, Int, Maybe Value)] ->
  [(Int, Int, Maybe Value)]
shredGo r d x [] acc = (r, d, Just x) : acc
shredGo r d x (Required k : pth) acc = case x ^? key k of
  Nothing -> error $ "Oh no! The key " ++ show k ++ " is required"
  Just x' -> shredGo r (d + 1) x' pth acc
shredGo r d x (Optional k : pth) acc = case x ^? key k of
  Nothing -> (r, d, Nothing) : acc
  Just x' -> shredGo r (d + 1) x' pth acc
shredGo r d x (Repeated k : pth) acc = case x ^? key k % _Array of
  Nothing -> error $ "Oh no! The key " ++ show k ++ " is required"
  Just xs
    | null xs -> (r, d, Nothing) : acc
    | otherwise ->
        mconcat
          [ shredGo i (d + 1) x pth []
            | (x, i) <- zip (toList xs) [0 ..]
          ]
          ++ acc

rawJSON :: Value
Just rawJSON =
  decode
    . fromString
    . unlines
    $ [ "[{",
        "\"owner\": \"Julien Le Dem\",",
        "\"ownerPhoneNumbers\": \"555 123 4567\",",
        "\"ownerPhoneNumbers\": \"555 666 1337\",",
        "\"contacts\": [{",
        "\"name\": \"Dmitriy Ryaboy\",",
        "\"phoneNumber\": \"555 987 6543\"",
        "},",
        "{",
        "\"name\": \"Chris Aniszczyk\"",
        "}",
        "]",
        "},",
        "{ \"owner\" : \"A. Nonymous\", \"contacts\" : [] }",
        "]"
      ]

myPath :: [PathKey]
myPath = [Repeated "contacts", Optional "phoneNumber"]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
