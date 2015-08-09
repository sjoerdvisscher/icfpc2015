{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import Logic
import Types
import Prelude hiding (id)

import qualified Data.ByteString.Lazy as BS
import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Traversable (for)
import System.Console.ANSI (clearScreen)
import System.Console.CmdArgs

data Args = Args
  { files :: [String]
  , timeLimit :: Int
  , memoryLimit :: Int
  , phrasesOfPower :: [String]
  , coreCount :: Int
  , uploadResults :: Bool
  } deriving (Show, Typeable, Data)

defArgs :: Args
defArgs = Args { files = def, timeLimit = def, memoryLimit = def, phrasesOfPower = [], coreCount = 1, uploadResults = False }

main :: IO ()
main = do
  theArgs <- cmdArgs defArgs
  inps <- traverse inputFromFile (files theArgs)
  let outs = inps >>= playSimple
  if uploadResults theArgs then
    upload outs
  else
    BS.putStr $ encode outs `mappend` "\n"

p0, p1, p4, p6, p10, p13, p23 :: Input
p0 = Input {id = 0, units = [Unit {members = [V {x = 0, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -2, y = 0},V {x = 2, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -1, y = -1},V {x = -1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 1, y = -1},V {x = -2, y = 0},V {x = 1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -1, y = -1},V {x = 2, y = 0},V {x = -1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = 2, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -2, y = 0},V {x = 0, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = 1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -1, y = -1},V {x = 0, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = 2, y = 0},V {x = 4, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -2, y = 0},V {x = 0, y = 0},V {x = 2, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -4, y = 0},V {x = -2, y = 0},V {x = 0, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = 1, y = 1},V {x = 0, y = 2}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -1, y = -1},V {x = 0, y = 0},V {x = -1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = -2},V {x = 1, y = -1},V {x = 0, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = -1, y = 1},V {x = 0, y = 2}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -1, y = -1},V {x = -2, y = 0},V {x = -1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = -2},V {x = -1, y = -1},V {x = 0, y = 0}], pivot = V {x = 0, y = 0}}], width = 20, height = 10, filled = [], sourceLength = 100, sourceSeeds = [0]}
p1 = Input {id = 1, units = [Unit {members = [V {x = 0, y = 0}], pivot = V {x = 0, y = 0}}], width = 30, height = 15, filled = [V {x = 4, y = 4},V {x = 6, y = 4},V {x = 8, y = 4},V {x = 10, y = 4},V {x = 12, y = 4},V {x = 22, y = 4},V {x = 5, y = 5},V {x = 17, y = 5},V {x = 23, y = 5},V {x = 4, y = 6},V {x = 22, y = 6},V {x = 5, y = 7},V {x = 7, y = 7},V {x = 9, y = 7},V {x = 17, y = 7},V {x = 23, y = 7},V {x = 4, y = 8},V {x = 18, y = 8},V {x = 22, y = 8},V {x = 5, y = 9},V {x = 17, y = 9},V {x = 4, y = 10},V {x = 6, y = 10},V {x = 8, y = 10},V {x = 10, y = 10},V {x = 12, y = 10},V {x = 18, y = 10},V {x = 22, y = 10}], sourceLength = 100, sourceSeeds = [0]}
p4 = Input {id = 4, units = [Unit {members = [V {x = 0, y = 0},V {x = -2, y = 0},V {x = 2, y = 0},V {x = 4, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = -2, y = 0},V {x = 2, y = 0},V {x = 3, y = -1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = -2, y = 0},V {x = 2, y = 0},V {x = 3, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = -2, y = 0},V {x = 2, y = 0},V {x = 1, y = -1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = -2, y = 0},V {x = 2, y = 0},V {x = 1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = -1, y = -1},V {x = 2, y = 0},V {x = 3, y = -1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = -2, y = 0},V {x = 1, y = -1},V {x = 3, y = -1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = -2, y = 0},V {x = 1, y = 1},V {x = 3, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = -2, y = 0},V {x = -1, y = -1},V {x = 1, y = -1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = -2, y = 0},V {x = 1, y = -1},V {x = 1, y = 1}], pivot = V {x = 0, y = 0}}], width = 20, height = 15, filled = [], sourceLength = 200, sourceSeeds = [0,16868,32001,13661,12352,29707,19957,2584,21791,18451,17818,26137,7533,29971,2895,177,8466,17014,23414,23008,15766,6045,13537,31051,12140,26930,28921,8444,29697,8269,12976,28635,16520,22345,22572,12272,6532,2148,23344,19542,22290,2586,19530,11006,8700,30014,21695,26153,13694,20701]}
p6 = Input {id = 6, units = [Unit {members = [V {x = 0, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = 2, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -2, y = 0},V {x = 0, y = 0},V {x = 2, y = 0}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = 0, y = 0},V {x = 2, y = 0},V {x = 1, y = 1}], pivot = V {x = 0, y = 0}},Unit {members = [V {x = -2, y = 0},V {x = 0, y = 0},V {x = 1, y = 1}], pivot = V {x = 0, y = 0}}], width = 20, height = 10, filled = [], sourceLength = 150, sourceSeeds = [0,13120,18588,31026,7610,25460,23256,19086,24334,22079,9816,8466,3703,13185,26906,16903,24524,9536,11993,21728,2860,13859,21458,15379,10919,7082,26708,8123,18093,26670,16650,1519,15671,24732,16393,5343,28599,29169,8856,23220,25536,629,24513,14118,17013,6839,25499,17114,25267,8780]}
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

uploadAll :: IO [()]
uploadAll = do
  ps <- allProblems
  for ps $ \p -> do
    print (id p)
    upload (playSimple p)

quickFix :: Input -> [Output]
quickFix p = makeOutput p (repeat cmds)
  where
    cmds = if length firstUnit == 1 then "ei!ia! ia!" else "ei!ia! ia!r'lyehyuggoth"
    firstUnit = rotations . head . head $ sources p

playSimple :: Input -> [Output]
playSimple p = makeOutput p $ map (snd . playSimple' (initialBoard p) "") $ sources p

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

replay :: Input -> Int -> [Char] -> IO ()
replay p sd = replay1 (sources1 p sd) b0
  where
    b0 = initialBoard p
    replay1 [] _ _ = return ()
    replay1 (u:us) b cs = do
      let rots = rotations u
      (b', cs') <- replayCs b rots (initialPos b rots) cs
      replay1 us b' cs'
    replayCs b u pr [] = if fits b u pr then fail "No more commands" else return (b, [])
    replayCs b u pr (c:cs) = do
      clearScreen
      let b' = place b u pr
      putStr $ showBoard b'
      _ <- getChar
      let pr' = move (powerMove c) pr
      if fits b u pr' then
        replayCs b u pr' cs
      else
        return (fullLines b', cs)
