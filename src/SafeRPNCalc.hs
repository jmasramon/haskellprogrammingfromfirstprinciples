module SafeRPNCalc where

import Data.List
import Control.Monad

solveRPN :: String -> Double
solveRPN =  head . foldl rpnFoldingFunc [] . words

-- looks like a state monad
rpnFoldingFunc :: [Double] -> String -> [Double]
rpnFoldingFunc (x:y:xs) "+" = (x+y):xs
rpnFoldingFunc (x:y:xs) "-" = (x-y):xs
rpnFoldingFunc (x:y:xs) "*" = (x*y):xs
rpnFoldingFunc xs numberStr = read numberStr:xs 

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(x,"")] -> Just x
    _ -> Nothing


rpnSafeFoldingFunc :: [Double] -> String -> Maybe [Double]
rpnSafeFoldingFunc (x:y:xs) "+" = Just $ (x+y):xs
rpnSafeFoldingFunc (x:y:xs) "-" = Just $ (x-y):xs
rpnSafeFoldingFunc (x:y:xs) "*" = Just $ (x*y):xs
rpnSafeFoldingFunc xs numberStr = liftM (:xs) (readMaybe numberStr) 

safeSolveRPN :: String -> Maybe Double
safeSolveRPN s = do
    [result] <- foldM rpnSafeFoldingFunc [] (words s)
    return result


safeSolveRPN' :: String -> Maybe Double
safeSolveRPN' = fmap head . foldM rpnSafeFoldingFunc [] . words