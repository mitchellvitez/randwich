{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import System.Random
import Data.ByteString

main :: IO ()
main = do
  g <- newStdGen
  quickHttpServe (echoHandler g)

echoHandler :: RandomGen g => g -> Snap()
echoHandler g = do
  let (rand, _) = sandwichPicker g
  let sandwichName = getSandwichFromNum rand
  let sandwichNumber = getNumAsBS rand
  let sentence = Data.ByteString.concat ["You should get the ", sandwichName, " (#", sandwichNumber, ")! It's really tasty!"]
  writeBS sentence

sandwichPicker :: RandomGen g => g -> (Int, g)
sandwichPicker g = randomR (1, 17) g

getNumAsBS :: Int -> ByteString
getNumAsBS x
  | x == 1 = "1"
  | x == 2 = "2"
  | x == 3 = "3"
  | x == 4 = "4"
  | x == 5 = "5"
  | x == 6 = "6"
  | x == 7 = "7"
  | x == 8 = "8"
  | x == 9 = "9"
  | x == 10 = "10"
  | x == 11 = "11"
  | x == 12 = "12"
  | x == 13 = "13"
  | x == 14 = "14"
  | x == 15 = "15"
  | x == 16 = "16"
  | x == 17 = "17"


getSandwichFromNum :: Int -> ByteString
getSandwichFromNum x
  | x == 1 = "Pepe"
  | x == 2 = "Big John"
  | x == 3 = "Totally Tuna"
  | x == 4 = "Turkey Tom"
  | x == 5 = "Vito"
  | x == 6 = "Vegetarian"
  | x == 7 = "Smoked Ham Club"
  | x == 8 = "Billy Club"
  | x == 9 = "Italian Night Club"
  | x == 10 = "Hunter's Club"
  | x == 11 = "Country Club"
  | x == 12 = "Beach Club"
  | x == 13 = "Gourmet Veggie Club"
  | x == 14 = "Bootlegger Club"
  | x == 15 = "Club Tuna"
  | x == 16 = "Club Lulu"
  | x == 17 = "Ultimate Porker"
