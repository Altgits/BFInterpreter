module Local.Eval(eval) where

import           Data.Char   (chr, ord)
import           Data.List   (foldl')
import           Local.Types

eval :: Env -> BFuckVal -> Env
-- Increases the current cell
eval env Increase   = do tape <- env
                         return $ focusApply (+ 1) tape
-- Decreases the current cell
eval env Decrease   = do tape <- env
                         return $ focusApply (subtract 1) tape
-- Moves the focus of the tape forward
eval env Forward    = do tape <- env
                         return $ tapeRight tape
-- Moves the focus of the tape backwards
eval env Backwards  = do tape <- env
                         return $ tapeLeft tape
-- Get the current cell then prints it out, returning tape intact
eval env Out        = do tape <- env
                         let curCell = getFocus tape
                         putChar $ chr curCell
                           -- Prints the cell's ASCII representation
                         return tape
-- Gets a character from stdin and turns it into its ASCII number, replacing the
-- cell in focus with that character's number
eval env In         = do (Tape left _ right) <- env
                         input <- ord <$> getChar  -- Gets the char and makes it into ASCII
                         return $ Tape left input right
-- Evaluates every value in the Loop, then checks the current cell to see if its zero, if
-- it is, it ends the loop, otherwise it evaluates the whole thing again with the new tape
eval env loop@(Loop vals) =
                      do let env' = foldl' eval env vals
                         oneRun <- env'
                         case getFocus oneRun of
                          0 -> env'
                          _ -> eval env' loop

