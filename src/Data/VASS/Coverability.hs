{- | Any coverability solver should be able to take in data as a CovProblem
    and output a CovResult.

    Readers will read data from a file, into the shape described here.
    This provides a unified interface for dealing with solvers 
    AND different filetypes.
-}
module Data.VASS.Coverability where

import Data.VASS



{-| We define a coverabiliy checker as a function which maps some 
    coverability problem ('CovProblem') to a result ('CovResult').

    We allow the checker to perform operations in IO on the way.
    (For example, you might want to maintain state with 'MVar', or 
    lookup or store data on disk.) -}
type CovChecker = CovProblem -> IO CovResult


{-| A coverability problem comprises the VASS itself, along with 
    the initial and final configurations which we intend to cover.
-}
data CovProblem = CovProblem 
    { system  :: VASS
    , initial :: Conf
    , target  :: Conf
    }
    deriving Show


{-| The result of a CovChecker can be either safe or unsafe. -}
data CovResult
    = Safe
    | Unsafe
    deriving Show