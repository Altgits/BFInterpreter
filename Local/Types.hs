module Local.Types( BFuckVal (..)
                  , Tape(..), Env
                  , focusApply
                  , tapeRight, tapeLeft
                  ) where

import           Data.List   (uncons)
import           Data.Word   (Word8)
import           Text.Parsec (ParseError)

-- Representation of every BFuck value
data BFuckVal = Forward
              | Backwards
              | Increase
              | Decrease
              | Loop [BFuckVal]
              | Out
              | In
              | Err ParseError
  deriving (Show)

-- Actual tape represented as a zipper
data Tape = Tape { getLeft  :: [Word8]
                 , getFocus :: Word8
                 , getRight :: [Word8]}

type Env = IO Tape

-- Zipper helpers
focusApply :: (Word8 -> Word8) -> Tape -> Tape
focusApply f (Tape l focus r) = Tape l (f focus) r

-- Reels left and right
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

