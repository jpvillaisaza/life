module Life

  where

-- array
import Data.Array

-- base
import Data.Bool
import System.IO


type Grid =
  Array (Integer, Integer) Bool

sh :: Grid -> String
sh x =
  unlines $ fmap (bool '-' 'X' . (x !)) <$> [[(i, j) | j <- [0..9]] | i <- [0..9]]

grid :: [(Integer, Integer)] -> Grid
grid s =
  array ((0, 0), (9,9)) [((i, j), (i, j) `elem` s) | i <- [0..9], j <- [0..9]]

neighbors :: (Integer, Integer) -> Grid -> [(Integer, Integer)]
neighbors c@(x, y) g =
  filter
    (\s -> s /= c && inRange (bounds g) s && g ! s)
    [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1]]

onneighbors :: (Integer, Integer) -> Grid -> Int
onneighbors c = length . neighbors c

evolve1 :: Grid -> Grid
evolve1 g =
  (//) g $ fmap sd $ assocs g
  where
    sd d@((i, j), a)
      | a && onneighbors (i,j) g < 2 = ((i,j), False)
      | a && onneighbors (i,j) g > 3 = ((i,j), False)
      | not a && onneighbors (i,j) g == 3 = ((i,j), True)
      | otherwise = d

evolve :: Grid -> IO ()
evolve g = do
  hFlush stdout
  hPutStrLn stdout $ sh g
  let g' = evolve1 g
  if g == g' then return () else evolve g'


g' =
  grid [(3,3),(3,2),(3,4)]
