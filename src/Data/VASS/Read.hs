-- | This module's parseFile function will attempt to parse a given
-- file in any way. If every option fails, it will exit.
module Data.VASS.Read (readAny) where

import Text.Megaparsec
import System.Exit (die)

import Data.VASS.Read.Shared
import Data.VASS.Read.MIST
import Data.VASS
import Data.Coverability

-- | Take a file representing a coverability problem and return 
-- the problem as a Haskell value.
readAny :: FilePath -> IO CovProblem
readAny path = do
    fileData <- readFile path
    let problemE = parse anyParser path fileData
    case problemE of
        Left errors -> die $ errorBundlePretty errors 
                            ++ "\n The file \"" ++ path 
                            ++ "\" could not be read."
        Right problem -> return problem

-- | At the moment there is only one parser. 
anyParser :: Parser CovProblem
anyParser = choice 
    [ readMIST
    ]