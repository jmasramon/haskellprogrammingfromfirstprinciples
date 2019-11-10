module Polish where

isNumber :: String -> Bool
isNumber str = 
  case (reads str) :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False

operate :: [Double] -> String -> [Double]
operate a b = if isNumber b 
              then (read b :: Double):a
              else case b of
                "+" -> ((+) (a!!1) (head a)):(snd $ splitAt 2 a)
                "-" -> ((-) (a!!1) (head a)):(snd $ splitAt 2 a)
                "*" -> ((*) (a!!1) (head a)):(snd $ splitAt 2 a)
                "/" -> ((/) (a!!1) (head a)):(snd $ splitAt 2 a)

solveRpn :: String -> Double
solveRpn op = head $ foldl operate [] opElems
  where opElems = words op

solveRpn' :: String -> Double
solveRpn' op = head $ foldl operate' [] opElems
  where opElems = words op
        operate' (x:y:xs) "+" = y + x : xs
        operate' (x:y:xs) "-" = y - x : xs
        operate' (x:y:xs) "*" = y * x : xs
        operate' (x:y:xs) "/" = y / x : xs
        operate' xs numStr = (read numStr) : xs
