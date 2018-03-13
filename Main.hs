module Main where

import           Control.Monad      (foldM_)
import qualified Data.Text.IO       as TI
import           Local.Eval
import           Local.Parse
import           Local.Types        (Tape (..))
import           System.Environment (getArgs)
import           System.IO

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> putStrLn "No file given"
            _ -> do let filename = head args
                    handle <- openFile filename ReadMode
                    cont <- TI.hGetContents handle
                    let program = readBF cont
                    foldM_ eval initTape program
                    hClose handle

initTape :: Tape
initTape = let list = repeat 0
            in Tape [] 0 list
