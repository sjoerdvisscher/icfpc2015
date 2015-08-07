module Logic where

import Random
import Types
import Data.Array.Unboxed

sources :: Input -> [[Unit]]
sources inp = source (units inp) (sourceLength inp) <$> sourceSeeds inp

source :: [Unit] -> Int -> Int -> [Unit]
source us len sd = map (\ix -> us !! (ix `mod` uslen)) $ take len (randoms sd)
  where
    uslen = length us

initialBoard :: Input -> Board
initialBoard inp = array b [(i, i `elem` filled inp)| i <- range b ]
  where
    b = (Cell 0 0, Cell (width inp - 1) (height inp - 1))

showBoard :: Board -> String
showBoard b = tail (concatMap render (assocs b)) ++ "\n"
  where
    render (c, fld) = sep c ++ if fld then "# " else "_ "
    sep (Cell 0 row) | odd row = "\n "
                     | otherwise = "\n"
    sep _ = ""
