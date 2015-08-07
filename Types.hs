{-# LANGUAGE DeriveGeneric #-}
module Types where

import GHC.Generics
import Data.Aeson
import Data.Ix
import Data.Array.Unboxed

type Board = UArray Cell Bool

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

data Cell = Cell { x :: Int, y :: Int } deriving (Show, Eq, Ord, Generic)
instance Ix Cell where
  range (Cell x0 y0, Cell x1 y1) = map (uncurry $ flip Cell) $ range ((y0, x0), (y1, x1))
  index (Cell x0 y0, Cell x1 y1) (Cell x2 y2) = index ((y0, x0), (y1, x1)) (y2, x2)
  inRange (Cell x0 y0, Cell x1 y1) (Cell x2 y2) = inRange ((y0, x0), (y1, x1)) (y2, x2)
  rangeSize (Cell x0 y0, Cell x1 y1) = rangeSize ((y0, x0), (y1, x1))

data Unit = Unit { members :: [Cell], pivot :: Cell } deriving (Show, Generic)

instance FromJSON Input
instance FromJSON Cell
instance FromJSON Unit

data Output = Output
  { problemId :: Int
  , seed :: Int
  , solution :: String
  }
  deriving (Show, Generic)

instance ToJSON Output
