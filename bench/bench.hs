module Main where

import Criterion.Main
import qualified MapReduce as MR

args = Args "tweet" Nothing "knicks"
main = defaultMain [
  bgroup "main"
    [
      bench "regex"  $ nfIO $ MR.runWith $ args (Just Pattern)
    , bench "indice" $ nfIO $ MR.runWith $ args (Just Indice)
    ]
  ]
