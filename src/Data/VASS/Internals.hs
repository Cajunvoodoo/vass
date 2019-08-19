{-| __This is an internal module. Compatibility will not be maintained.__

    This file defines helper functions which are used in the @vass@ library. 
    They are not part of the official API!
-}
module Data.VASS.Internals where

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Foldable  (toList)
import Data.Bifunctor (second)

{-| This is the inverse of 'decollate'; Given a multimap of keys to values,
    collect together all the values. 
    
    The runtime efficiency of this is dependent on your choice of semigroup.
    Don't use lists for long data as @<>@ is maximally inefficient on lists,
    but for small amounts of data it should be fine.

    >>> collate [(1, 'a'), (1, 'b')] :: Map Integer (Vector Char)
    Map.fromList [(1, Vector.fromList ['a','b'])]
-}
collate :: (Ord k, Eq a, Semigroup (f a), Applicative f) 
        => [(k, a)] -> Map k (f a)
collate l = Map.fromListWith (<>) $ second pure <$> l


{-| This function takes vectors of elements stored in a map and returns
    all the keys paired with everything out of their associated vector.

    >>> decollate $ Map.fromList [(1, Vector.fromList ['a','b'])]
    [(1,'a'), (2,'b')] 
-}
decollate :: (Ord k, Eq a, Foldable f) => Map k (f a) -> [(k, a)]
decollate m = [ (a, b) | (a,as) <- Map.assocs m, b <- toList as]