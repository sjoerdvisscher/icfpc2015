module Logic where

import Random
import Types
import Data.Array.Unboxed
import Data.Monoid

sources :: Input -> [[Unit]]
sources inp = source (units inp) (sourceLength inp) <$> sourceSeeds inp

source :: [Unit] -> Int -> Int -> [Unit]
source us len sd = map (\ix -> us !! (ix `mod` uslen)) $ take len (randoms sd)
  where
    uslen = length us

simplifyInput :: Input -> Input
simplifyInput inp = inp {
    units = map simplifyUnit (units inp),
    filled = map shift (filled inp),
    width = 2 * width inp
  }
  where
    simplifyUnit (Unit cs piv) = Unit (map (subtract (shift piv) . shift) cs) 0
    shift (Cell x0 y0) | odd y0 = Cell (2*x0 + 1) y0
                       | otherwise = Cell (2*x0) y0

initialBoard :: Input -> Board
initialBoard inp = array b [ (i, i `elem` filled inp) | i <- range b ]
  where
    b = (Cell 0 0, Cell (width inp - 1) (height inp - 1))

showBoard :: Board -> String
showBoard b = tail (concatMap render (assocs b)) ++ "\n"
  where
    render (c@(Cell x0 y0), fld) | odd x0 == odd y0 = sep c ++ if fld then "X" else "_"
                                 | otherwise = sep c ++ " "
    sep (Cell 0 _) = "\n"
    sep _ = ""

initialPos :: Board -> Unit -> Cell
initialPos board (Unit cs _) = Cell x0' y0
  where
    y0 = negate (minimum (map y cs))
    bwidth = x (snd $ bounds board) `div` 2
    xs = map x cs
    left = minimum xs `div` 2
    right = maximum xs `div` 2
    x0 = (bwidth - right - left) `div` 2
    left' = left + x0
    right' = bwidth - (right + x0)
    x0' = x0 * 2 + if odd y0 then if left' < right' then 1 else -1 else 0

fits :: Board -> Unit -> Cell -> Bool
fits board (Unit cs _) pos = not . getAny $ foldMap (Any . (board !) . (+ pos)) cs

place :: Board -> Unit -> Cell -> Board
place board (Unit cs _) pos = board // [ (c + pos, True) | c <- cs ]

rotateCellCW, rotateCellCCW :: Cell -> Cell
rotateCellCW  (Cell x0 y0) = Cell ((x0 - 3 * y0) `div` 2) ((y0 + x0) `div` 2)
rotateCellCCW (Cell x0 y0) = Cell ((x0 + 3 * y0) `div` 2) ((y0 - x0) `div` 2)

rotateCW, rotateCCW :: Unit -> Unit
rotateCW  (Unit cs _) = Unit (map rotateCellCW cs) 0
rotateCCW (Unit cs _) = Unit (map rotateCellCCW cs) 0
