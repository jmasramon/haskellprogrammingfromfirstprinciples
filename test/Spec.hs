import Test.QuickCheck
import Test.QuickCheck.Instances
import Lib
import Data.Text as T

prop_punctuationInvariant text = curate text == 
                                  curate noPuncText
  where noPuncText = removePuntuation text

prop_reverseInvariant text = isPalindrome text ==
                              (isPalindrome . T.reverse) text

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
  putStrLn "done !"