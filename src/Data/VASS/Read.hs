-- | This module's 'readAny' function will attempt to parse a given
-- file with any of the built-in tools. If every option fails, it will exit.
module Data.VASS.Read where

import Text.Megaparsec
import System.Exit (die)

import Data.VASS.Read.Shared
import Data.VASS.Read.MIST
import Data.VASS.Coverability
import Data.VASS

{-| Take a file representing a coverability problem and return 
    the problem as a Haskell value.
    If you want to define your own parser, replace 'anyParser' with 
    your own parser(s) and call 'readAny\'' directly.
-}
readAny :: FilePath -> IO CovProblem
readAny = readAny' anyParser

{-| Parser-independent version of readAny. -}
readAny' :: Parser CovProblem -> FilePath -> IO CovProblem
readAny' parser path = do
    fileData <- readFile path
    let problemE = parse anyParser path fileData
    case problemE of
        Left errors -> die $ errorBundlePretty errors 
                            ++ "\n The file \"" ++ path 
                            ++ "\" could not be read."
        Right problem -> return problem

{-| A combination of each of the built-in parsers. 
    Each will be tried in turn. If none of them succeed, an error will
    be thrown by 'readAny\''.
-}
anyParser :: Parser CovProblem
anyParser = choice [ readMIST ]