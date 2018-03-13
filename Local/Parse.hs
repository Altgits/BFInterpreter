{-# LANGUAGE FlexibleContexts #-}
module Local.Parse(readBF) where

import           Local.Types
import           Text.Parsec
import           Text.Parsec.Text (Parser)

readBF input = case parse (many parseBF) "BFuck" input of
                Right val -> val

parseBF :: Parser BFuckVal
-- Ignores everything that cannot be parsed, then tries each possible value
parseBF = do skipMany $ noneOf "><+-.,[]"
             choice (parseLoop : singleBF)

singleBF :: [Parser BFuckVal]
-- Every possible standalone BF command
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

