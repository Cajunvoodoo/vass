{- | Any coverability solver should be able to take in data as a CovProblem
    and output a CovResult.

    Readers will read data from a file, into the shape described here.
    This provides a unified interface for dealing with solvers 
    AND different filetypes.
-}
module Data.VASS.Coverability where

import Data.VASS


data CovResult
    = Safe
    | Unsafe
    deriving Show

data CovProblem = CovProblem 
    { system  :: VASS
    , initial :: Conf
    , target  :: Conf
    }
    deriving Show

type CovChecker = CovProblem -> IO CovResult
