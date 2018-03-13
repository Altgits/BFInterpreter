module Local.Types( BFuckVal (..)
                  , Tape(..), Env
                  , focusApply
                  , tapeRight, tapeLeft
                  ) where

import           Data.Int  (Int8)
import           Data.List (uncons)

-- Representation of every BFuck value
data BFuckVal = Forward
              | Backwards
              | Increase
              | Decrease
              | Loop [BFuckVal]
              | Out
              | In
  deriving (Show)

-- Actual tape represented as a zipper
data Tape = Tape { getLeft  :: [Int8]
                 , getFocus :: Int8
                 , getRight :: [Int8]}

type Env = IO Tape

-- Zipper helpers
focusApply :: (Int8 -> Int8) -> Tape -> Tape
focusApply f (Tape l focus r) = Tape l (f focus) r

tapeLeft :: Tape -> Tape
tapeLeft tape@(Tape _ _ [])            = wrapL tape
  where wrapL (Tape l foc r) = let list = reverse l ++ (foc:r)
                                   Just (nFoc, rest) = uncons list
                                in Tape [] nFoc rest
tapeLeft (Tape left oldF (newF:restR)) = Tape (oldF : left) newF restR

tapeRight :: Tape -> Tape
tapeRight tape@(Tape [] _ _)             = wrapR tape
  where wrapR (Tape l foc r) = let list = reverse r ++ (foc:l)
                                   Just (nFoc, rest) = uncons list
                                in Tape rest nFoc []
tapeRight (Tape (newF:restL) oldF right) = Tape restL newF (oldF:right)

