{-# LANGUAGE OverloadedStrings #-}

module Main where

import Logic
import Types
import Prelude hiding (id)

import qualified Data.ByteString.Lazy as BS
import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Traversable (for)

p0, p1 :: Input
p0 = Input {id = 0, units = [Unit {members = [Cell {x = 0, y = 0}], pivot = Cell {x = 0, y = 0}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 2, y = 0}], pivot = Cell {x = 1, y = 0}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 0, y = 2}], pivot = Cell {x = 0, y = 1}},Unit {members = [Cell {x = 2, y = 0},Cell {x = 0, y = 1},Cell {x = 2, y = 2}], pivot = Cell {x = 1, y = 1}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 1, y = 1},Cell {x = 0, y = 2}], pivot = Cell {x = 0, y = 1}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 1, y = 0}], pivot = Cell {x = 0, y = 0}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 1, y = 0}], pivot = Cell {x = 1, y = 0}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 0, y = 1}], pivot = Cell {x = 0, y = 0}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 0, y = 1}], pivot = Cell {x = 0, y = 1}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 1, y = 0},Cell {x = 2, y = 0}], pivot = Cell {x = 0, y = 0}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 1, y = 0},Cell {x = 2, y = 0}], pivot = Cell {x = 1, y = 0}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 1, y = 0},Cell {x = 2, y = 0}], pivot = Cell {x = 2, y = 0}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 0, y = 1},Cell {x = 0, y = 2}], pivot = Cell {x = 0, y = 0}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 0, y = 1},Cell {x = 0, y = 2}], pivot = Cell {x = 0, y = 1}},Unit {members = [Cell {x = 0, y = 0},Cell {x = 0, y = 1},Cell {x = 0, y = 2}], pivot = Cell {x = 0, y = 2}},Unit {members = [Cell {x = 1, y = 0},Cell {x = 0, y = 1},Cell {x = 1, y = 2}], pivot = Cell {x = 1, y = 0}},Unit {members = [Cell {x = 1, y = 0},Cell {x = 0, y = 1},Cell {x = 1, y = 2}], pivot = Cell {x = 1, y = 1}},Unit {members = [Cell {x = 1, y = 0},Cell {x = 0, y = 1},Cell {x = 1, y = 2}], pivot = Cell {x = 1, y = 2}}], width = 10, height = 10, filled = [], sourceLength = 100, sourceSeeds = [0]}
p1 = Input {id = 1, units = [Unit {members = [Cell {x = 0, y = 0}], pivot = Cell {x = 0, y = 0}}], width = 15, height = 15, filled = [Cell {x = 2, y = 4},Cell {x = 3, y = 4},Cell {x = 4, y = 4},Cell {x = 5, y = 4},Cell {x = 6, y = 4},Cell {x = 11, y = 4},Cell {x = 2, y = 5},Cell {x = 8, y = 5},Cell {x = 11, y = 5},Cell {x = 2, y = 6},Cell {x = 11, y = 6},Cell {x = 2, y = 7},Cell {x = 3, y = 7},Cell {x = 4, y = 7},Cell {x = 8, y = 7},Cell {x = 11, y = 7},Cell {x = 2, y = 8},Cell {x = 9, y = 8},Cell {x = 11, y = 8},Cell {x = 2, y = 9},Cell {x = 8, y = 9},Cell {x = 2, y = 10},Cell {x = 3, y = 10},Cell {x = 4, y = 10},Cell {x = 5, y = 10},Cell {x = 6, y = 10},Cell {x = 9, y = 10},Cell {x = 11, y = 10}], sourceLength = 100, sourceSeeds = [0]}

-- inputFromFile "problems/problem_0.json"

inputFromFile :: String -> IO Input
inputFromFile s = do
  contents <- BS.readFile s
  either fail return (eitherDecode contents)

allProblems :: IO [Input]
allProblems = for [0..23::Int] $ \i -> inputFromFile ("problems/problem_" ++ show i ++ ".json")

makeOutput :: Input -> [String] -> [Output]
makeOutput inp = zipWith (Output (id inp)) (sourceSeeds inp)

upload :: [Output] -> IO ()
upload out = do
  let opts = defaults
              & auth ?~ basicAuth "" "bg4PXjnbbA8E0dOqaq2FySO3bqTSt940sTRfluARiMQ="
              & header "Content-Type" .~ ["application/json"]
  r <- postWith opts "https://davar.icfpcontest.org/teams/58/solutions" (encode out)
  print r

main :: IO ()
main = do
  print $ sources p0

printAllBoards :: IO [()]
printAllBoards = allProblems >>= traverse (putStrLn . showBoard . initialBoard)

powers :: [String]
powers = ["ei!", "ia!", "r'lyeh", "yuggoth", "satan"]
