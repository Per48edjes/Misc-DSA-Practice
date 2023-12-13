module Util where

import Data.List (tails)

pairwiseCombinations :: [a] -> [(a, a)]
pairwiseCombinations xs = [(x, y) | (x : ys) <- tails xs, y <- ys]
