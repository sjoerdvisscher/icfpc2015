{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Types where

import Prelude hiding (id)
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Network.Wreq
import Control.Lens

data Input = Input
  { id :: Int
  , units :: [Unit]
  , width :: Int
  , height :: Int
  , filled :: [Cell]
  , sourceLength :: Int
  , sourceSeeds :: [Int]
  }
  deriving (Show, Generic)

data Cell = Cell { x :: Int, y :: Int } deriving (Show, Eq, Generic)
data Unit = Unit { members :: [Cell], pivot :: Cell } deriving (Show, Generic)

instance FromJSON Input
instance FromJSON Cell
instance FromJSON Unit

problem_0 :: IO Input
problem_0 = inputFromFile "problems/problem_0.json"

inputFromFile :: String -> IO Input
inputFromFile s = do
  contents <- BS.readFile s
  either fail return (eitherDecode contents)

data Output = Output
  { problemId :: Int
  , seed :: Int
  , solution :: String
  }
  deriving (Show, Generic)

instance ToJSON Output

makeOutput :: Input -> [String] -> [Output]
makeOutput inp = zipWith (Output (id inp)) (sourceSeeds inp)

upload :: [Output] -> IO ()
upload out = do
  let opts = defaults
              & auth ?~ basicAuth "" "bg4PXjnbbA8E0dOqaq2FySO3bqTSt940sTRfluARiMQ="
              & header "Content-Type" .~ ["application/json"]
  r <- postWith opts "https://davar.icfpcontest.org/teams/58/solutions" (encode out)
  print r
