module Cup
  ( cup
  , getOz
  , drink
  , isEmpty
  , manySips)
  where

-- cup :: Int -> a -> Int
cup flOz message = message flOz

-- getOz :: (a -> Int) -> Int
getOz aCup = aCup id

drink aCup ozDrank = if ozDiff > 0
  then cup ozDiff
  else cup 0
  where flOz = getOz aCup
        ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0

manySips = flip (foldl drink) [1, 1, 1, 1, 1]



