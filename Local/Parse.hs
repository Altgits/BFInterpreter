{-# LANGUAGE FlexibleContexts #-}
module Local.Parse(parseBFProg) where

import           Local.Types
import           Text.Parsec
import           Text.Parsec.Text (Parser)

parseBFProg :: Parser [BFuckVal]
parseBFProg = many parseBF

parseBF :: Parser BFuckVal
parseBF = do skipMany $ noneOf "><+-.,[]"
             choice (parseLoop : singleBF)

singleBF :: [Parser BFuckVal]
singleBF = [ parseBFChar '>' Forward
           , parseBFChar '<' Backwards
           , parseBFChar '+' Increase
           , parseBFChar '-' Decrease
           , parseBFChar '.' Out
           , parseBFChar ',' In
           ]
  where parseBFChar c val = do _ <- char c
                               return val

parseLoop :: Parser BFuckVal
parseLoop = do _ <- char '['
               vals <- many parseBF
               _ <- char ']'
               return $ Loop vals
