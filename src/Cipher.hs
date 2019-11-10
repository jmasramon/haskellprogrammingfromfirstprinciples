module Cipher where

import Data.Char (ord)

caesar::Int->String->String
caesar n s = map (((!!) alphabet) . (flip mod (length alphabet)) . (+ (n-index)) . ord) s
  where alphabet = ['a'..'z']
        index = (ord 'a')

caesar'::Int->String->String
caesar' n s = [(((!!) alphabet) . (flip mod (length alphabet)) . (+ (n-index)) . ord) c | c<-s]
  where alphabet = ['a'..'z']
        index = (ord 'a')

unCaesar::Int->String->String 
unCaesar n s = caesar (26-n) s