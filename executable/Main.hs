{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified MapReduce
import Matcher
import Options.Generic

instance ParseField Matcher.Matcher

instance ParseRecord MapReduce.Args where
  parseRecord = parseRecordWithModifiers modifiers

modifiers :: Modifiers
modifiers = defaultModifiers { fieldNameModifier = drop 1 }


main :: IO ()
main = do
    args <- getRecord "ETL Shtuff"
    MapReduce.runWith args
