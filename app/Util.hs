module Util where

import Data.Char (isSpace)
import Data.List (intersect)

replace :: Eq a => a -> [a] -> [a] -> [a]
replace _ _ [] = []
replace src dest (x:xs)
  | src == x = dest ++ replace src dest xs
  | otherwise = x : replace src dest xs

trimLead :: String -> String
trimLead "" = ""
trimLead (x:xs)
  | isSpace x = trimLead xs
  | otherwise = x:xs

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs = null . intersect xs
