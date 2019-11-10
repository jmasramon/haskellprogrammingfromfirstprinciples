module Lib
    ( assert
    , curate
    , isPalindrome
    , removePuntuation
    ) where

import Data.Char as C (toLower, isPunctuation)
import Data.Text as T (map, filter, reverse,Text)


assert :: Bool -> String -> String -> IO ()
assert predicate positiveOutputText negativeOutputText = if predicate
                                                            then putStrLn positiveOutputText
                                                            else putStrLn negativeOutputText

allToLower :: T.Text -> T.Text
allToLower = (T.map C.toLower)
                                                            
removePuntuation :: T.Text -> T.Text
removePuntuation text = T.filter (not . C.isPunctuation) text

curate :: T.Text -> T.Text
curate text = (allToLower . removePuntuation) text

isPalindrome :: T.Text -> Bool
isPalindrome text = curatedText == T.reverse curatedText   
    where curatedText = curate text                                                         
