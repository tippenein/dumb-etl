{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards #-}

module MapReduce where

import Matcher

import Data.Foldable (traverse_)
import System.FilePath.Glob (compile, globDir1)

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified ListT
import qualified STMContainers.Map as Map
import qualified Data.List as List
import Data.Maybe (fromMaybe, fromJust)
import GHC.Generics (Generic)
import Data.Ord (comparing)

data Args = Args { _input :: FilePath
                 , _output :: Maybe FilePath
                 , _search :: String
                 , _pattern :: String
                 , _matcher :: Maybe Matcher
                 } deriving (Generic, Show)


intToBs :: Integer -> BS.ByteString
intToBs = T.encodeUtf8 . T.pack . show

outputBuilder :: BS.ByteString -> (Char8.ByteString, Integer) -> BS.ByteString
outputBuilder r (k, v) =
    (BS.concat
            [ r
            , Char8.toStrict k
            , "\t"
            , intToBs v
            , "\n"
            ])

writeOut :: Maybe FilePath -> BS.ByteString -> IO ()
writeOut (Just f) = BS.writeFile f
writeOut Nothing = BS.putStr

runWith :: Args -> IO ()
runWith Args{..} = do
    print $ "searching: " ++ _search
    files <- globDir1 (compile _pattern) _input

    m <- Map.newIO

    let increment key = STM.atomically $ do
            x <- Map.lookup key m
            case x of
                Nothing -> Map.insert 1 key m
                Just n  -> n' `seq` Map.insert n' key m  where n' = n + 1

    let matcher = fromMaybe Indice _matcher
    print $ "running: " ++ show matcher ++ " matcher"

    let processFile file = Async.Concurrently (do
            bytes <- LazyBS.readFile file
            traverse_ increment (parseFile (getMatcher matcher) bytes) )

    Async.runConcurrently (traverse_ processFile files)

    let sorted = STM.atomically (
          List.foldl' outputBuilder "" <$>
          List.sortBy (flip $ comparing snd) <$>
          (ListT.toReverseList (Map.stream m))
          )
    writeOut _output =<< sorted

    where
      getMatcher :: Matcher -> (LazyBS.ByteString -> Bool)
      getMatcher Indice = indiceMatch searchBS
      getMatcher Pattern = regexMatch searchBS

      searchBS = T.encodeUtf8 $ T.pack _search
