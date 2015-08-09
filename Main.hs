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

p0, p1, p10, p13, p23 :: Input
p0 = Input {id = 0, units = [Unit {members = [V {x = 0, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -2, y = 0},V {x = 2, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -1, y = -1},V {x = -1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 1, y = -1},V {x = -2, y = 0},V {x = 1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -1, y = -1},V {x = 2, y = 0},V {x = -1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = 2, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -2, y = 0},V {x = 0, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = 1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -1, y = -1},V {x = 0, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = 2, y = 0},V {x = 4, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -2, y = 0},V {x = 0, y = 0},V {x = 2, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -4, y = 0},V {x = -2, y = 0},V {x = 0, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = 1, y = 1},V {x = 0, y = 2}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -1, y = -1},V {x = 0, y = 0},V {x = -1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = -2},V {x = 1, y = -1},V {x = 0, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = -1, y = 1},V {x = 0, y = 2}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -1, y = -1},V {x = -2, y = 0},V {x = -1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = -2},V {x = -1, y = -1},V {x = 0, y = 0}], pivot = V {x = 0, y = 0}}], width = 20, height = 10, filled = [], sourceLength = 100, sourceSeeds = [0]}
p1 = Input {id = 1, units = [Unit {members = [V {x = 0, y = 0}], pivot = V {x = 0, y = 0}}], width = 30, height = 15, filled = [V {x = 4, y = 4},V {x = 6, y = 4},V {x = 8, y = 4},V {x = 10, y = 4},V {x = 12, y = 4},V {x = 22, y = 4},V {x = 5, y = 5},V {x = 17, y = 5},V {x = 23, y = 5},V {x = 4, y = 6},V {x = 22, y = 6},V {x = 5, y = 7},V {x = 7, y = 7},V {x = 9, y = 7},V {x = 17, y = 7},V {x = 23, y = 7},V {x = 4, y = 8},V {x = 18, y = 8},V {x = 22, y = 8},V {x = 5, y = 9},V {x = 17, y = 9},V {x = 4, y = 10},V {x = 6, y = 10},V {x = 8, y = 10},V {x = 10, y = 10},V {x = 12, y = 10},V {x = 18, y = 10},V {x = 22, y = 10}], sourceLength = 100, sourceSeeds = [0]}
p10 = Input {id = 10, units = [Unit {members = [V {x = 4, y = 0},V {x = 6, y = 0}], pivot = V {x = 0, y = 0}}], width = 20, height = 7, filled = [V {x = 1, y = 1},V {x = 5, y = 1},V {x = 9, y = 1},V {x = 13, y = 1},V {x = 17, y = 1},V {x = 3, y = 5},V {x = 5, y = 5},V {x = 7, y = 5},V {x = 9, y = 5},V {x = 11, y = 5},V {x = 13, y = 5},V {x = 15, y = 5},V {x = 17, y = 5}], sourceLength = 100, sourceSeeds = [0]}
p13 = Input {id = 13, units = [Unit {members = [V {x = -2, y = -2},V {x = 0, y = -2},V {x = 2, y = -2},V {x = -3, y = -1},V {x = -1, y = -1},V {x = 1, y = -1},V {x = 3, y = -1},V {x = -4, y = 0},V {x = 0, y = 0},V {x = 4, y = 0},V {x = -3, y = 1},V {x = -1, y = 1},V {x = 1, y = 1},V {x = 3, y = 1},V {x = -4, y = 2},V {x = 0, y = 2},V {x = 4, y = 2}], pivot = V {x = 0, y = 0}}], width = 30, height = 20, filled = [], sourceLength = 100, sourceSeeds = [0]}
p23 = Input {id = 23, units = [Unit {members = [V {x = -2, y = 0},V {x = 0, y = 0},V {x = 2, y = 0}], pivot = V {x = 0, y = 0}}], width = 20, height = 9, filled = [V {x = 3, y = 3},V {x = 5, y = 3},V {x = 7, y = 3},V {x = 9, y = 3},V {x = 11, y = 3},V {x = 13, y = 3},V {x = 15, y = 3},V {x = 17, y = 3},V {x = 3, y = 5},V {x = 5, y = 5},V {x = 7, y = 5},V {x = 9, y = 5},V {x = 11, y = 5},V {x = 13, y = 5},V {x = 15, y = 5},V {x = 17, y = 5},V {x = 3, y = 7},V {x = 5, y = 7},V {x = 7, y = 7},V {x = 9, y = 7},V {x = 11, y = 7},V {x = 13, y = 7},V {x = 15, y = 7},V {x = 17, y = 7}], sourceLength = 100, sourceSeeds = [0]}

-- inputFromFile "problems/problem_0.json"

inputFromFile :: String -> IO Input
inputFromFile s = do
  contents <- BS.readFile s
  either fail (return . simplifyInput) (eitherDecode contents)

allProblems :: IO [Input]
allProblems = for [0..24::Int] $ \i -> inputFromFile ("problems/problem_" ++ show i ++ ".json")

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

uploadAll :: IO ()
uploadAll = do
  ps <- allProblems
  let sols = ps >>= playSimple
  upload sols

quickFix :: Input -> [Output]
quickFix p = makeOutput p (repeat cmds)
  where
    cmds = if length firstUnit == 1 then "ei!ia! ia!" else "ei!ia! ia!r'lyehyuggoth"
    firstUnit = rotations . head . head $ sources p

playSimple :: Input -> [Output]
playSimple p = makeOutput p $ map (snd . playSimple' (initialBoard p) "") $ sources p

playSimple' :: Board -> String -> [Unit] -> (Board, String)
playSimple' b s [] = (b, s)
playSimple' b s (u:us) = maybe (b, s) (\(b', s') -> playSimple' b' (s ++ s') us) $ playUnit b (rotations u)

printAllBoards :: IO [()]
printAllBoards = allProblems >>= traverse (putStrLn . showBoard . initialBoard)

printAllBoardsWithFirstUnit :: IO [()]
printAllBoardsWithFirstUnit = allProblems >>= traverse (putStrLn . showBoard . initialBoardWithFirstUnit)

initialBoardWithFirstUnit :: Input -> Board
initialBoardWithFirstUnit p = place b firstUnit pos
  where
    firstUnit = rotations . head . head $ sources p
    b = initialBoard p
    pos = initialPos b firstUnit

play :: Input -> IO ()
play p = do
  putStrLn (showBoard b')
  print s
  where
    us = head $ sources p
    b = initialBoard p
    (b', s) = playSimple' b "" us
