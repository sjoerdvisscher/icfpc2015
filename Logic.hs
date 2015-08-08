{-# LANGUAGE FlexibleContexts #-}
module Logic where

import Random
import Types
import Data.Array.Unboxed
import Data.Monoid
import qualified Data.Set as Set

sources :: Input -> [[Unit]]
sources inp = source (members <$> units inp) (sourceLength inp) <$> sourceSeeds inp

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
    shift (V x0 y0) | odd y0 = V (2*x0 + 1) y0
                    | otherwise = V (2*x0) y0

initialBoard :: Input -> Board
initialBoard inp = array b [ (i, False) | i <- range b ] // map (\i -> (i, True)) (filled inp)
  where
    b = (V 0 0, V (width inp - 1) (height inp - 1))

showBoard :: Board -> String
showBoard b = tail (concatMap render (assocs b)) ++ "\n"
  where
    render (c@(V x0 y0), fld) | odd x0 == odd y0 = sep c ++ if fld then "X" else "_"
                                 | otherwise = sep c ++ " "
    sep (V 0 _) = "\n"
    sep _ = ""

initialPos :: Board -> Unit -> V
initialPos board cs = V x0' y0
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

fits :: Board -> Unit -> V -> Bool
fits board cs pos = not . getAny $ foldMap (Any . (board ?) . (+ pos)) cs

place :: Board -> Unit -> V -> Board
place board cs pos = board // [ (c + pos, True) | c <- cs ]

rotateVCW, rotateVCCW :: V -> V
rotateVCW  (V x0 y0) = V ((x0 - 3 * y0) `div` 2) ((y0 + x0) `div` 2)
rotateVCCW (V x0 y0) = V ((x0 + 3 * y0) `div` 2) ((y0 - x0) `div` 2)

rotateCW, rotateCCW :: Unit -> Unit
rotateCW  = map rotateVCW
rotateCCW = map rotateVCCW

rotations :: Unit -> [Unit]
rotations u0 = u0 : (takeWhile (neqUnit u0) . tail $ iterate rotateCW u0)

neqUnit :: Unit -> Unit -> Bool
neqUnit u0 u1 = Set.fromList u0 /= Set.fromList u1

(?) :: (IArray a Bool, Ix i) => a i Bool -> i -> Bool
arr ? i = not (inRange (bounds arr) i) || (arr ! i)

powers :: [String]
powers = ["ei!", "ia! ia!", "r'lyeh", "yuggoth"]

powerMove :: Char -> Move
powerMove c
  | c `elem` "p'!.03" = MoveW
  | c `elem` "bcefy2" = MoveE
  | c `elem` "aghij4" = MoveSW
  | c `elem` "lmno 5" = MoveSE
  | c `elem` "dqrvz1" = RotateCW
  | c `elem` "kstuwx" = RotateCCW
powerMove _ = error "unvalid power character"
