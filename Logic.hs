{-# LANGUAGE FlexibleContexts #-}
module Logic where

import Random
import Types hiding (id, x, y)
import qualified Types as V (x, y)
import Data.Array.Unboxed
import Data.Monoid
import qualified Data.Set as Set
import Control.Monad
import Control.Applicative

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

initialPos :: Board -> [Unit] -> PosRot
initialPos board rots = PosRot (V x0' y0) 0 (length rots)
  where
    cs = head rots
    y0 = negate (minimum (map V.y cs))
    bwidth = V.x (snd $ bounds board) `div` 2
    xs = map V.x cs
    left = minimum xs `div` 2
    right = maximum xs `div` 2
    x0 = (bwidth - right - left) `div` 2
    left' = left + x0
    right' = bwidth - (right + x0)
    x0' = x0 * 2 + if odd y0 then if left' < right' then 1 else -1 else 0

fits :: Board -> [Unit] -> PosRot -> Bool
fits board rots (PosRot pos r _) = not . getAny $ foldMap (Any . (board ?) . (+ pos)) (rots !! r)

place :: Board -> [Unit] -> PosRot -> Board
place board rots (PosRot pos r _) = fullLines h $ board // [ (c + pos, True) | c <- rots !! r ]
  where
    (_, V w h) = bounds board
    fullLines :: Int -> Board -> Board
    fullLines (-1) b = b
    fullLines y b = if isFull y b then fullLines y (clearLine y b) else fullLines (y - 1) b
    isFull :: Int -> Board -> Bool
    isFull y b = let o = offset y in and [ b ! V x y | x <- [o, o + 2..w] ]
    clearLine :: Int -> Board -> Board
    clearLine 0 b = b // [ (V x 0, False) | x <- [0, 2..w] ]
    clearLine y b = let o = offset y in clearLine (y - 1) $ b // [ (V x y, b ! V (x + 1 - 2*o) (y - 1)) | x <- [o, o + 2..w] ]

offset :: Int -> Int
offset y = if odd y then 1 else 0

clearLine :: Int -> Int -> Board -> Board
clearLine w 0 b = b // [ (V x 0, False) | x <- [0, 2..w] ]
clearLine w y b = let o = offset y in clearLine w (y - 1) $ b // [ (V x y, b ! V (x + 1 - 2*o) (y - 1)) | x <- [o, o + 2..w] ]

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

moveSets :: [(String, [Move])]
moveSets = map (\s -> (s, map powerMove s)) (reverse powers) ++ map (\c -> ([c], [powerMove c])) "pbaldk"

move :: Move -> PosRot -> PosRot
move MoveE (PosRot p r m) = PosRot (p + V 2 0) r m
move MoveW (PosRot p r m) = PosRot (p - V 2 0) r m
move MoveSE (PosRot p r m) = PosRot (p + V 1 1) r m
move MoveSW (PosRot p r m) = PosRot (p + V (-1) 1) r m
move RotateCW (PosRot p r m) = PosRot p ((r + 1) `mod` m) m
move RotateCCW (PosRot p r m) = PosRot p ((r - 1) `mod` m) m

doMove :: (PosRot -> Bool) -> Move -> (BoardVisited, PosRot) -> Maybe (BoardVisited, PosRot)
doMove prd m (b, pr) = if (b ? pr') || not (prd pr') then Nothing else Just (b // [(pr', True)], pr')
  where
    pr' = move m pr

doMoveSet :: (PosRot -> Bool) -> [Move] -> (BoardVisited, PosRot) -> Maybe (BoardVisited, PosRot)
doMoveSet prd = foldr ((>=>) . doMove prd) return

tryAllMoves :: (PosRot -> Bool) -> (BoardVisited, PosRot) -> (PosRot, String)
tryAllMoves prd bpr0 = (pr', moves)
  where
    tryOneMove :: [(String, [Move])] -> (BoardVisited, PosRot) -> Maybe ((BoardVisited, PosRot), String)
    tryOneMove [] _ = Nothing
    tryOneMove ((s, ms):mss) bpr = ((\bpr' -> (bpr', s)) <$> doMoveSet prd ms bpr) <|> tryOneMove mss bpr

    keepTrying :: ((BoardVisited, PosRot), String) -> ((BoardVisited, PosRot), String)
    keepTrying (bpr, s) = maybe (bpr, s) (\(bpr', s') -> keepTrying (bpr', s ++ s')) $ tryOneMove moveSets bpr

    ((_, pr'), moves) = keepTrying $ keepTrying $ keepTrying $ keepTrying (bpr0, "")


mkBoardVisited :: Board -> PosRot -> BoardVisited
mkBoardVisited b initPos@(PosRot p _ m) = array bds [ (i, False) | i <- range bds ] // [(initPos, True)]
  where
    bds = (PosRot (p - V w 0) 0 m, PosRot (p + V w (h + 1)) (m - 1) m)
    (_, V w h) = bounds b

playUnit :: Board -> [Unit] -> Maybe (Board, String)
playUnit b rots = if fits' initPos then Just (b', moves ++ " ") else Nothing
  where
    fits' = fits b rots
    initPos = initialPos b rots
    (pr, moves) = tryAllMoves fits' (mkBoardVisited b initPos, initPos)
    b' = place b rots pr
