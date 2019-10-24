{-| Vector Addition Systems with States (VASS) are a common formalism for
    modelling concurrent computations. Petri Nets are an equivalent formalism.

    This library provides an interface both for reading common VASS file 
    formats, and for representing VASS in Haskell data types.

    Note that this library does not provide a type for Vector Addition Systems 
    (VAS) on their own, as VASS can trivially represent them with a singleton 
    state. 

    Some of the readers used by "Data.VASS.Read" ingest VAS formats and will 
    automatically lift to VASS. When this happens, we name this singleton state 
    @μ@.
-}
module Data.VASS (
  -- * Vector Addition Systems with States (VASS)
  VASS(..),
  -- * Associated Data Types
  Transition(..), 
  Configuration (..),
  Conf, 
  -- * Helper Types
  Name(..), State, Place, 
) where


--------------------------------------------------------------------------------

-- Containers
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.String

-- Typeclasses & Control Flow
import Control.Applicative
import Data.Foldable
import Data.Bifunctor (second)
import Data.Monoid
import Data.Semigroup
import Data.Void
import Data.Coerce

-- Local imports
import Data.VASS.Internals


--------------------------------------------------------------------------------

{-| Our principal data type. 

    Note that we store the @dimension@ of the vector even though we could query
    the number of places as @Vector.length places@. This might look like a 
    violation of single-source-of-truth. But given that @places@ and 
    @transitions@ both encode the information themselves, it makes sense to 
    have a singular place to lookup the "true" dimension of the system in case 
    something goes wrong. -}
data VASS = VASS
    { 
      -- | How many places are in the net 
      -- (this must be equal to the length of 'places')
      dimension   :: !Integer 
      -- | The ordered vector of labels of places. 
      -- This describes the order of indices in transitions and configurations.
    , places      :: !(Vector (Name Place))
      -- | The unordered set of all states in the system.
    , states      :: !(Set (Name State))
      -- | A mapping from given states to given transitions.
    , transitions :: !(Map (Name State) (Vector Transition))
    }
    deriving (Eq, Show)



{-| 'Name' is a phantom type for annotating something with a string label.

    @State@ and @Place@ are <https://wiki.haskell.org/Empty_type unit types> 
    used as markers for the @Name@ phantom type.
    
    eg. @Name Place@ is a string representing a named VASS coordinate
    (equivalently, a Petri Net place). -}
newtype Name a = Name String
    deriving (Eq, Ord, Semigroup, Monoid, IsString)


instance Show (Name a) where 
    {-| We can use string literals to define a Name because of 
        the OverloadedString extension and the IsString typeclass. -}
    show (Name n) = show n


-- | Simple unit type for use with 'Name'.
data State = State
-- | Simple unit type for use with 'Name'.
data Place = Place



{-| A transition comprises a subtractive vector __Pre__, an additive vector
    __Post__, and the state to move to. The 'VASS' describes the mapping from
    states to vectors of transitions. -}
data Transition = Transition 
    { name      :: !(Name Transition)
    , pre       :: !(Vector Integer)
    , post      :: !(Vector Integer)
    , nextState :: !(Name State)
    } 
    deriving (Eq, Show)



{-| A configuration comprises the current state of a VASS and the current
    vector (total function from place to number of tokens). -}
data Configuration a = Configuration
    { state :: !(Name State)
    , vec   :: !(Vector a)
    }
    deriving (Eq, Show)



{-| Conf is the typical instantiation of Configuration.

    Note that the type used ('Integer') does NOT enforce that values are 
    non-negative. This is because some algorithms require using below-zero 
    values, such as when constructing /pre-runs/ (runs where values are not 
    bounded below).
    These applications would require considerable boilerplate
    and data marshalling. It is recommended that users define a
    vector subtraction function which performs zero-checks. -}
type Conf = Configuration Integer