module Data.Coverability where

import Data.VASS

--------------------------------------------------------------------------------
-- * Coverability Problems

{- | Any coverability solver should be able to take in data as a CovProblem
    and output a CovResult.

    Readers will read data from a file, into the shape described here.
    This provides a unified interface for dealing with solvers 
    AND different filetypes.
-}

data CovResult
    = Safe
    | Unsafe
    deriving Show

data CovProblem = CovProblem 
    { initial :: Conf
    , target  :: Conf
    , system  :: VASS
    }

type CovChecker = CovProblem -> IO CovResult
