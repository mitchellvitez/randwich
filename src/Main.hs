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
  quickHttpServe echoHandler

echoHandler :: Snap()
echoHandler = do
  let rand = getSandwichNum
  let str = getSandwichFromNum rand
  writeBS str

getSandwichNum :: IO Int
getSandwichNum = getStdRandom (randomR (1,17))

getSandwichFromNum :: IO Int -> ByteString
getSandwichFromNum x
  | x == 1 = "Sandwich one"
  | x == 2 = "Sandwich two"
  | x == 3 = "Sandwich th"
  | x == 4 = "Sandwich fo"
  | x == 5 = "Sandwich fi"
  | x == 6 = "Sandwich two"
  | x == 7 = "Sandwich two"
  | x == 8 = "Sandwich two"
  | x == 9 = "Sandwich two"
  | x == 10 = "Sandwich two"
  | x == 11 = "Sandwich two"
  | x == 12 = "Sandwich two"
  | x == 13 = "Sandwich two"
  | x == 14 = "Sandwich two"
  | x == 15 = "Sandwich two"
  | x == 16 = "Sandwich two"
  | x == 17 = "Sandwich two"
