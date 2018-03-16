{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Local.Parse(readBF) where

import           Data.Maybe       (catMaybes)
import           Local.Types
import           Text.Parsec
import           Text.Parsec.Text (Parser)

readBF input = case parse parseBF "BFuck" input of
  Right vals -> vals
  Left err   -> [Err err]

parseBF :: Parser [BFuckVal]
parseBF = fmap catMaybes . many $   -- Gets multiple maybe vals, then gets them out of Maybe
  (Just <$> choice bfVals) <|> comment -- These are the pssible parses, either something I can
    where comment = do _ <- noneOf "]"  -- use or a comment
                       return Nothing

bfVals :: [Parser BFuckVal]
-- Every possible standalone BF command
bfVals = [ parseBFChar '>' Forward
         , parseBFChar '<' Backwards
         , parseBFChar '+' Increase
         , parseBFChar '-' Decrease
         , parseBFChar '.' Out
         , parseBFChar ',' In
         , parseLoop
         ]
  where parseBFChar c val = do _ <- char c
                               return val

parseLoop :: Parser BFuckVal
-- A loop is defined as a bunch of values inside two brackets
parseLoop = do _ <- char '['
               vals <- parseBF
               _ <- char ']'
               return $ Loop vals
