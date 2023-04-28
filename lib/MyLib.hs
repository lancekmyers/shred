{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Data.Aeson
import Data.Aeson.KeyMap (fromList, insert, lookup)
import Data.Aeson.Optics (AsValue (_Array), key)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (Foldable (..))
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as BV
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V
import Optics

-- menu items label
data PathKey
  = Required Key
  | Optional Key
  | Repeated Key
  deriving (Show)

shred ::
  (FromJSON a) =>
  Value ->
  [PathKey] ->
  BV.Vector (Int, Int, Maybe a)
shred (Array xs) col = V.concat $ (\x -> shredGo 0 0 x col V.empty) <$> (toList xs)
shred x col = shredGo 0 0 x col V.empty

shredGo ::
  (FromJSON a, Vector v (Int, Int, Maybe a)) =>
  Int ->
  Int ->
  Value ->
  [PathKey] ->
  v (Int, Int, Maybe a) ->
  v (Int, Int, Maybe a)
shredGo r d x [] acc = case fromJSON x of
  Success a -> V.cons (r, d, Just a) acc
  Error err -> error err
shredGo r d x (Required k : pth) acc = case x ^? key k of
  Nothing -> error $ "Oh no! The key " ++ show k ++ " is required"
  Just x' -> shredGo r (d + 1) x' pth acc
shredGo r d x (Optional k : pth) acc = case x ^? key k of
  Nothing -> V.cons (r, d, Nothing) acc
  Just x' -> shredGo r (d + 1) x' pth acc
shredGo r d x (Repeated k : pth) acc = case x ^? key k % _Array of
  Nothing -> error $ "Oh no! The key " ++ show k ++ " is required"
  Just xs
    | null xs -> V.cons (r, d, Nothing) acc
    | otherwise ->
        V.concat
          [ shredGo i (d + 1) x pth V.empty
            | (x, i) <- zip (toList xs) (r : repeat (r + 1))
          ]
          V.++ acc

unshred ::
  ToJSON a =>
  [PathKey] ->
  BV.Vector (Int, Int, Maybe a) ->
  Value
unshred pth col = BV.foldl' go (Array []) col
  where
    ins :: Key -> Value -> Value -> Value
    ins k v (Object m) = Object (insert k v m)
    ins k v m = m
    go ::
      ToJSON a =>
      Value ->
      (Int, Int, Maybe a) ->
      Value
    go val (r, d, x) =
      let (accer, mker) = splitOnRepLevel r pth
       in let rec2 = mkRecord mker (toJSON <$> x)
           in over (mkTrav accer % _Array) (flip BV.snoc rec2) val

splitOnRepLevel :: Int -> [PathKey] -> ([PathKey], [PathKey])
splitOnRepLevel r xs = go r xs []
  where
    go 0 xs acc = (reverse acc, xs)
    go r (Repeated k : xs) acc = go (r - 1) xs (Repeated k : acc)
    go r (k' : xs) acc = go r xs (k' : acc)
    go _ [] acc = (reverse acc, [])

mkTrav :: [PathKey] -> AffineTraversal' Value Value
mkTrav [] = atraversal Right (\_ x -> x)
mkTrav [Repeated k] = key k
mkTrav (Repeated k : ks) = key k % _Array % _last % (mkTrav ks)
mkTrav (Optional k : ks) = key k % (mkTrav ks)
mkTrav (Required k : ks) = key k % (mkTrav ks)

mkRecord :: [PathKey] -> Maybe Value -> Value
mkRecord ks x = foldr go (maybe Null id x) ks
  where
    go (Required k) val = Object (fromList [(k, val)])
    go (Optional k) val = Object (fromList [(k, val)])
    go (Repeated k) val = Object (fromList [(k, Array $ BV.singleton val)])

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
        "},",
        "{",
        "\"name\": \"Chris Aniszczyk\",",
        "\"phoneNumber\": \"555 666 1347\"",
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
