{-# LANGUAGE Strict #-}
module Local.Eval(eval) where

import           Control.Monad (foldM)
import           Data.Char     (chr, ord)
import           Local.Types

eval :: Tape -> BFuckVal -> Env
{-# INLINE eval #-}
-- Increases the current cell
eval tape Increase   = return $ focusApply (+ 1) tape
-- Decreases the current cell
eval tape Decrease   = return $ focusApply (subtract 1) tape
-- Moves the focus of the tape forward
eval tape Forward    = return $ tapeRight tape
-- Moves the focus of the tape backwards
eval tape Backwards  = return $ tapeLeft tape
-- Get the current cell then prints it out, returning tape intact
eval tape Out        = do let curCell = getFocus tape
                          putChar $ chr (fromIntegral curCell)
                            -- Prints the cell's ASCII representation
                          return tape
-- Gets a character from stdin and turns it into its ASCII number, replacing the
-- cell in focus with that character's number
eval tape In         = do let (Tape left _ right) = tape
                          input <- fromIntegral . ord <$> getChar
                             -- Gets the char and makes it into ASCII
                          return $ Tape left input right
-- Evaluates every value in the Loop, then checks the current cell to see if its zero, if
-- it is, it ends the loop, otherwise it evaluates the whole thing again with the new tape
eval tape loop@(Loop vals) =
                      do let tape' = foldM eval tape vals
                         oneRun <- tape'
                         case getFocus oneRun of
                          0 -> tape'
                          _ -> eval oneRun loop
eval tape (Err err) = do print err
                         return tape

