{-# LANGUAGE FlexibleContexts #-}
module Local.Parse where

import           Text.Parsec
import           Text.Parsec.Text (Parser)

data BFuckVal = Forward
              | Backwards
              | Increase
              | Decrease
              | Loop [BFuckVal]
              | Out
              | In
  deriving (Show)

parseBF :: Parser BFuckVal
parseBF = do skipMany $ noneOf "><+-.,[]"
             choice (parseLoop : singleBF)

singleBF :: [Parser BFuckVal]
singleBF = [ pBFVal '>' Forward
           , pBFVal '<' Backwards
           , pBFVal '+' Increase
           , pBFVal '-' Decrease
           , pBFVal '.' Out
           , pBFVal ',' In
           ]
  where pBFVal c val = do _ <- char c
                          return val

parseLoop :: Parser BFuckVal
parseLoop = do _ <- char '['
               vals <- many parseBF
               _ <- char ']'
               return $ Loop vals

