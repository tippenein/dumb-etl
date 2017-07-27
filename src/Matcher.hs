{-# LANGUAGE OverloadedStrings #-}
module Matcher where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Search as Search
import qualified Text.Regex.TDFA.ByteString as Regex
import Data.Text (Text)
import Text.Regex.TDFA.Common
import Data.Maybe (isJust)
import Data.Typeable (Typeable)

data Matcher = Indice | Pattern
  deriving (Show, Read, Typeable)

parseFile
    :: (LazyBS.ByteString -> Bool)
    -> LazyBS.ByteString
    -> [LazyBS.ByteString]
parseFile matchFunction bytes0 =
    [ neighborhood line
    | line <- Char8.lines bytes0
    , matchFunction line
    ]
  where
    neighborhood =
          LazyBS.takeWhile (/= 9)
        . LazyBS.drop 1
        . LazyBS.dropWhile (/= 9)

regexPattern :: BS.ByteString -> Regex.Regex
regexPattern bs = case Regex.compile comp ex bs of
                    Left err -> error (show err)
                    Right reg -> reg
  where
    comp = CompOption { caseSensitive = False
                      , multiline = False
                      , rightAssoc = True
                      , newSyntax = False
                      , lastStarGreedy = False
                      }
    ex = ExecOption { captureGroups = False}

regexMatch :: BS.ByteString -> LazyBS.ByteString -> Bool
regexMatch phrase bs =
    case regexMatch' bs of
      Left _ -> False
      Right m -> isJust m
  where
    regexMatch' =
          Regex.execute (regexPattern phrase)
        . LazyBS.toStrict

indiceMatch :: BS.ByteString-> LazyBS.ByteString -> Bool
indiceMatch phrase =
      not
    . null
    . Search.indices phrase
    . BS.map lower
    . LazyBS.toStrict

  where
    lower w8 = if 65 <= w8 && w8 < 91 then w8 + 32 else w8
