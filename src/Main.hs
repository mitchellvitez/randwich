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

echoHandler :: (RandomGen g) => g -> Snap()
echoHandler g = do
  let (rand, g) = sandwichPicker g
  let str = getSandwichFromNum rand
  writeBS str

sandwichPicker :: (RandomGen g) => g -> (Int, g)
sandwichPicker g = randomR (1, 17) g

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
