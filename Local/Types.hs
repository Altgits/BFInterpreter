module Local.Types( BFuckVal (..)
                  , Tape(..), Env
                  , focusApply
                  , tapeRight, tapeLeft
                  ) where

data BFuckVal = Forward
              | Backwards
              | Increase
              | Decrease
              | Loop [BFuckVal]
              | Out
              | In
  deriving (Show)

data Tape = Tape { getLeft  :: [Int]
                 , getFocus :: Int
                 , getRight :: [Int]}

type Env = IO Tape

focusApply :: (Int -> Int) -> Tape -> Tape
focusApply f (Tape l focus r) = Tape l (f focus) r

tapeLeft :: Tape -> Tape
tapeLeft tape@(Tape _ _ [])            = tape
tapeLeft (Tape left oldF (newF:restR)) = Tape (oldF : left) newF restR

tapeRight :: Tape -> Tape
tapeRight tape@(Tape [] _ _)             = tape
tapeRight (Tape (newF:restL) oldF right) = Tape restL newF (oldF:right)
