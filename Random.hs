module Random (randoms) where

import System.Random (RandomGen(..))
import Data.Bits
import Data.List

instance RandomGen Int where
  next seed = ((seed `shiftR` 16) .&. 0x7FFF, seed * 1103515245 + 12345)
  split seed = (seed, seed)

randoms :: Int -> [Int]
randoms = unfoldr (Just . next)

test :: [Int]
test = take 10 $ randoms 17
