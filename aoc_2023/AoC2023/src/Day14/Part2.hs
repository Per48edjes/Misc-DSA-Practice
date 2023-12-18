module Day14.Part2 where

import Data.Array (Array)
import Day14.Part1
import Util

calculateLoad' :: Array Coord Cell -> Int
calculateLoad' arr = (calculateRightLoad . rotateRight <$> spins) !! i
  where
    spins = iterate spin arr
    i = case floydTortoiseAndHare spins of
        Nothing -> error "No cycle found"
        Just (entryPoint, cycleLength) -> entryPoint + ((1000000000 - entryPoint) `mod` cycleLength)

-- | Apply the spin transformation to the platform
spin :: Array Coord Cell -> Array Coord Cell
spin = tiltRight . tiltDown . tiltLeft . tiltUp

-- | Tilt platform upwards
tiltUp :: Array Coord Cell -> Array Coord Cell
tiltUp = rotateLeft . tiltRight . rotateRight

-- | Tilt platform downwards
tiltDown :: Array Coord Cell -> Array Coord Cell
tiltDown = rotateRight . tiltRight . rotateLeft

-- | Tilt platform to the left
tiltLeft :: Array Coord Cell -> Array Coord Cell
tiltLeft = rotateLeft . rotateLeft . tiltRight . rotateRight . rotateRight
