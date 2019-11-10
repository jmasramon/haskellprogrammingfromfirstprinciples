module TimeSeries where
  
import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe
import Control.Applicative

file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2,  199.5), (3, 199.4)
        , (4, 198.9), (5,  199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]
file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]
file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]
file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]

data TS a = TS [Int] [Maybe a]

instance Show a => Show (TS a) where
  show (TS times values) = "[" ++ content ++ "]"
    where pairs = zipWith showTSPair times values
          content = intercalate ", " pairs

showTSPair :: Show a => Int -> (Maybe a) -> String
showTSPair i ma = "(" ++ show i ++ ", " ++ (show ma) ++ ")"

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes maybes
  where maybes = map (\time -> Map.lookup time mappedValues) completeTimes
        completeTimes = [minimum times .. maximum times]
        mappedValues = Map.fromList (zip times values)

createTSFromList::[(Int, a)] -> TS a
createTSFromList list = createTS times values
  where (times, values) = unzip list

-- instance Semigroup (TS a) where
--   (<>) = combineTS

-- combineTS :: TS a -> TS a -> TS a
-- combineTS (TS [] []) (TS times values) = TS times values 
-- combineTS (TS times values) (TS [] []) = TS times values 
-- combineTS (TS times values) (TS times' values') = TS (times ++ times) (values ++ values') 
  
instance Semigroup (TS a) where
  (<>) (TS [] []) (TS times values) = TS times values 
  (<>) (TS times values) (TS [] []) = TS times values 
  (<>) (TS times values) (TS times' values') = TS times'' values''
    where mappedTS1 = Map.fromList (zip times values)
          fullMap = foldl (\map (key, value) -> Map.insert key value map) mappedTS1 (zip times' values') 
          (times'', values'') = (unzip (Map.toList fullMap))
  
instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

mean :: Real a => [a] -> Double
mean xs = total/count
  where total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

meanTS :: Real a => TS a -> Maybe Double
-- meanTS mempty = Nothing
meanTS (TS times values) | (length times) == 0 && (length values) == 0 = Nothing
                         | allNothing (TS times values) = Nothing
                         | otherwise = Just $ mean $ map fromJust $ filter isJust values

allNothing:: TS a -> Bool
allNothing (TS times values) = and (map isNothing values)

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare cf = newCFunc
  where newCFunc (t1, Nothing) (t2, Nothing) = (t1, Nothing)
        newCFunc (_, Nothing) (t2, v2) = (t2, v2)
        newCFunc (t1, v1) (_, Nothing) = (t1, v1)
        newCFunc (t1, Just v1) (t2, Just v2) = if cf v1 v2 == v1 
                                               then (t1, Just v1)
                                               else (t2, Just v2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS cf (TS times values) = if all isNothing values
                                  then Nothing
                                  else Just (foldl tsCf (0, Nothing) pairs)
  where tsCf = makeTSCompare cf
        pairs = (zip times values)

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)
-- diffPair = liftA2 (-)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = if all isNothing values 
                          then TS times values
                          else TS times (Nothing:diffVals)
  where shiftedVals = tail values
        diffVals = zipWith diffPair shiftedVals values

meanMaybes :: Real a => [Maybe a] -> Maybe Double
meanMaybes vals = if any isNothing vals
                  then Nothing
                  else Just avg
  where avg = mean (map fromJust vals)

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg values n = if length valsToAvg == n
                      then (meanMaybes valsToAvg) : (movingAvg restVals n)
                      else []
  where valsToAvg = take n values
        restVals = tail values

movAvgTS :: (Real a) => TS a -> Int -> TS Double
movAvgTS (TS times values) n = TS times (nothings ++ avgs ++ nothings)
  where avgs = movingAvg values n
        nothings = replicate (n `div` 2) Nothing