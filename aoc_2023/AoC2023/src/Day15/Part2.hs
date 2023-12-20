module Day15.Part2 (totalFocusingPower) where

import Control.Monad.State
import Data.Char (isLetter)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Day15.Part1

-- Types

data Operation = Set | Unset
type FocalLength = Int
type Index = Int

type Box = (Map Text Index, Seq (Text, FocalLength))
type Boxes = V.Vector Box
type BoxesState = State Boxes Int

-- Functions

totalFocusingPower :: [Text] -> Int
totalFocusingPower steps = last $ evalState (mapM processBox steps) (V.replicate 256 (M.empty, Seq.empty))

-- | Manages the state of the boxes and calculates the focusing power of the current state
processBox :: Text -> BoxesState
processBox step = do
  boxes <- get
  let (indexMap, lenses) = boxes V.! boxNum
      (label, (op, mfL)) = processStep step
      boxNum = hash label
  case op of
    Set
      | M.member label indexMap -> do
          let index = indexMap M.! label
              focalLength = case mfL of
                Just fL -> fL
                Nothing -> error "Invalid focal length for set operation unavailable"
          put $ V.update boxes $ V.singleton (boxNum, (M.insert label index indexMap, Seq.update index (label, focalLength) lenses))
      | otherwise -> do
          let index = Seq.length lenses
              focalLength = case mfL of
                Just fL -> fL
                Nothing -> error "Invalid focal length for set operation unavailable"
          put $ V.update boxes $ V.singleton (boxNum, (M.insert label index indexMap, lenses |> (label, focalLength)))
    Unset
      | M.member label indexMap -> do
          let index = indexMap M.! label
              indexMap' = M.delete label $ M.map (\v -> v - 1) (M.filter (> index) indexMap) `M.union` indexMap
          put $ V.update boxes $ V.singleton (boxNum, (indexMap', Seq.deleteAt index lenses))
      | otherwise -> put boxes
  gets calculateFocusingPower

-- | Calculates the focusing power of the current state
calculateFocusingPower :: Boxes -> Int
calculateFocusingPower boxes = sum $ calculateFocusingPower' <$> boxes
 where
  calculateFocusingPower' :: Box -> Int
  calculateFocusingPower' (indexMap, lenses) = sum $ calculateFocusingPower'' <$> lenses
   where
    calculateFocusingPower'' (label, focalLength) =
      let
        boxNum = hash label
        index = indexMap M.! label
       in
        (boxNum + 1) * (index + 1) * focalLength

-- | Processes a step and returns the label, operation and focal length
processStep :: Text -> (Text, (Operation, Maybe FocalLength))
processStep step = (label, (op', focalLength'))
 where
  (label, rest) = case T.groupBy (\c1 c2 -> isLetter c1 && isLetter c2) step of
    l : r -> (l, r)
    _ -> error "Invalid step"
  (op', focalLength') = case rest of
    [op, focalLength] -> case TR.decimal focalLength of
      Right (fL, _) -> (toOp op, Just fL)
      Left _ -> error "Invalid focal length"
    [op] -> (toOp op, Nothing)
    _ -> error $ "Invalid op or focal length" ++ show rest

toOp :: Text -> Operation
toOp "=" = Set
toOp "-" = Unset
toOp _ = error "Invalid operation"
