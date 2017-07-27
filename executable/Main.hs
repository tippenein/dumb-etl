{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified MapReduce
import Matcher
import Options.Generic

instance ParseField Matcher.Matcher
instance ParseRecord MapReduce.Args


-- dumb-etl --input FILE/DIR --output FILE --search TERM --matcher

main :: IO ()
main = do
    args <- getRecord "ETL Shtuff"
    MapReduce.runWith args
