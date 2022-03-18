module Utils where

import qualified Data.List as L

unique :: (Eq a, Ord a) => [a] -> [a]
unique = fmap head . L.group . L.sort

