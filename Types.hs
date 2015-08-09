{-# LANGUAGE
    DeriveGeneric
  , DeriveTraversable
  , TypeFamilies
  , FlexibleInstances
  , UndecidableInstances
  , MultiParamTypeClasses
  #-}
module Types where

import GHC.Generics
import Data.Aeson
import Data.Ix
import Data.Array.Unboxed

type Board = UArray Cell Bool

data Input = Input
  { id :: Int
  , units :: [Unit']
  , width :: Int
  , height :: Int
  , filled :: [Cell]
  , sourceLength :: Int
  , sourceSeeds :: [Int]
  }
  deriving (Show, Generic)

data V = V { x :: {-# UNPACK #-} !Int, y :: {-# UNPACK #-} !Int } deriving (Show, Eq, Ord, Generic)
instance Ix V where
  range (V x0 y0, V x1 y1) = map (uncurry $ flip V) $ range ((y0, x0), (y1, x1))
  index (V x0 y0, V x1 y1) (V x2 y2) = index ((y0, x0), (y1, x1)) (y2, x2)
  inRange (V x0 y0, V x1 y1) (V x2 y2) = inRange ((y0, x0), (y1, x1)) (y2, x2)
  rangeSize (V x0 y0, V x1 y1) = rangeSize ((y0, x0), (y1, x1))
instance Num V where
  fromInteger i = V (fromInteger i) (fromInteger i)
  V a b + V c d = V (a + c) (b + d)
  V a b - V c d = V (a - c) (b - d)
  (*) = undefined
  abs = undefined
  signum = undefined

type Cell = V
data Unit' = Unit { members :: Unit, pivot :: Cell } deriving (Show, Generic)
type Unit = [Cell]

instance FromJSON Input
instance FromJSON V
instance FromJSON Unit'

data Output = Output
  { problemId :: Int
  , seed :: Int
  , solution :: String
  }
  deriving (Show, Generic)

instance ToJSON Output

data Move = MoveW | MoveE | MoveSW | MoveSE | RotateCW | RotateCCW deriving (Show, Enum, Bounded)

data PosRot = PosRot { prPos :: V, prRot :: Int, prMod :: Int } deriving (Show, Eq, Ord, Ix)
type BoardVisited = UArray PosRot Bool

data Game = Game {
  gboard :: Board,
  gscore :: Int,
  gsource :: [Unit],
  gsolution :: [Move]
}
