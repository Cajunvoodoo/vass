module Data.VASS where

-- | Datatypes
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Graph as Tree
import Data.Graph (Tree)
import Data.String
import Data.Void

-- | Typeclasses & Control Flow
import Control.Applicative
import Data.Foldable
import Data.Bifunctor (second)
import Data.Monoid
import Data.Semigroup
import Data.Coerce

import Text.Megaparsec hiding (State)


--------------------------------------------------------------------------------
-- * General Definitions

-- | A name is just a label attached to something.
newtype Name a = Name String
    deriving (Eq, Ord, Show, Semigroup, Monoid, IsString)

data State = State
data Place = Place


data Transition = Transition 
    { name      :: Name Transition
    , pre       :: Vector Integer
    , post      :: Vector Integer
    , nextState :: Name State
    } 
    deriving (Eq, Show)


data Configuration a = Configuration
    { state :: Name State
    , vec   :: Vector a
    }
    deriving (Eq, Show)

type Conf = Configuration Integer


--------------------------------------------------------------------------------
-- * Vector Addition Systems with States (VASS)

{- |A VASS is a VAS extended to include state. They are equal to VASs in
    expressivity.

    Most algorithms and consider VASSs rather than VASs, for the sake of
    generality.
-}

data VASS = VASS
    { dimension   :: Integer
    , places      :: Vector (Name Place)
    , states      :: Vector (Name State)
    , transitions :: Map (Name State) (Vector Transition)
    }
    deriving (Eq, Show)


-- Given a VASS, swap all its transitions round.
reverse :: VASS -> VASS
reverse v@VASS{..} = v 
    { transitions = collate . map rearrange . decollate $ transitions }

    where
        decollate :: (Ord k, Eq a) 
            => Map k (Vector a) 
            -> [(k, a)]
        decollate m = [ (a, b) | (a,as) <- Map.toList m, b <- toList as]

        collate :: (Ord k, Eq a, Semigroup (f a), Applicative f) 
            => [(k, a)] 
            -> Map k (f a)
        collate l = Map.fromListWith (<>) $ second pure <$> l

        rearrange :: (Name State, Transition) 
            -> (Name State, Transition)
        rearrange (s, Transition name pre post s') 
            = (s', Transition name post pre s)