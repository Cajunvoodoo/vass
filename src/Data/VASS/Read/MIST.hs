{- | This module is not intended to be used directly. Please see 
     'Data.VASS.Read' for the intended invocation. You may find this
     module useful for your own purposes though, so it is exposed in the 
     usual way.
-}
module Data.VASS.Read.MIST where

-- Containers
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Control flow
import Text.Megaparsec.Char (space1)
import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Maybe
import Control.Applicative (liftA2)
import Data.Coerce
import Data.Functor ((<&>))
import Data.Function ((&))

import Text.Megaparsec.Debug

import Data.VASS
import Data.VASS.Coverability
import Data.VASS.Read.Shared

{-| A parser for the MIST file format.
    This is an attempt to faithfully implement the specification as described on 
    the MIST wiki (<https://github.com/pierreganty/mist/wiki>). 
    However there may be some corner cases which are not covered. Please report
    any omissions on this project's issue tracker on GitHub.
-}
readMIST :: Parser CovProblem
readMIST = do
    places <- Vector.fromList <$> sectionVars
    rules  <- Vector.fromList <$> sectionRules

    initMap   <- sectionInit
    targetMap <- sectionTarget

    let transitions  = Map.insert "μ" (makeTransitions places rules) Map.empty
        initial      = Configuration "μ" $ makeDense initMap places
        target       = Configuration "μ" $ makeDense targetMap places
        states       = Set.fromList ["μ"]

    let system = VASS (fromIntegral $ length places) places states transitions
    return $ CovProblem {..}


{-| Convert a mapping from name->value to an indexable vector. 
    This is needed as our VASS format uses vectors but the MIST format 
    uses mappings. 
-}
makeDense :: Map (Name Place) Integer -> Vector (Name Place) -> Vector Integer
makeDense sparse = fmap (\p -> Map.findWithDefault 0 p sparse)

--------------------------------------------------------------------------------
-- * RULES

{- | Rules are written as a combination of preconditions and deltas.
    This format is strange but it can be easily translated to the correct
    [pre, post] transition vectors.
-}
data Rule = Rule 
    { conditions :: Map (Name Place) Integer
    , deltas     :: Map (Name Place, Char) Integer
    }
    deriving Show

{- | We convert the format from sparse to contiguous. This is quite
    an inefficient representation when the place-transition ratio is high, but
    it is the simplest representation to manipulate. 
-}
makeTransitions :: Vector (Name Place) -> Vector Rule -> Vector Transition
makeTransitions places rules = let

    makeContiguous Rule{..} = Transition 
        { name = ""
        , pre  = project '-'
        , post = project '+'
        , nextState = "μ"
        }
        where
        project :: Char -> Vector Integer
        project sign = (\p -> Map.findWithDefault 0 (p, sign) deltas) <$> places


    setName n x = x { name = n }

    transitions = makeContiguous <$> rules 

    addPlaceholderNames = Vector.zipWith ($) 
        (Vector.fromList 
            [setName $ coerce $ "t_" ++ show n | n <- [0..length rules-1]])

    in addPlaceholderNames transitions



--------------------------------------------------------------------------------
-- * PARSER

sectionVars :: Parser [Name Place]
sectionVars =  do
    keyword "vars"
    many $ coerce <$> try identifier


sectionRules :: Parser [Rule]
sectionRules = do
    keyword "rules"
    many $ try rule

sectionInit :: Parser (Map (Name Place) Integer)
sectionInit = do
    keyword "init"
    vals <- pair "=" `sepBy` comma
    return $ Map.fromList vals

sectionTarget :: Parser (Map (Name Place) Integer)
sectionTarget = do
    keyword "target"
    vals <- pair ">=" `sepBy` comma
    return $ Map.fromList vals    


pair :: String -> Parser (Name Place, Integer)
pair op = do
    place <- coerce <$> identifier
    symbol_ op
    val   <- integer
    return (place, val)

rule :: Parser Rule
rule = do
    conditions <- pair ">=" `sepBy` comma

    symbol "->"

    deltas <- (`sepBy` comma) $ do
        ident <- identifier
        symbol_ "\'"
        symbol_ "="
        symbol_ ident
        op <- oneOf ("+-"::String)
        amt <- integer
        return ((coerce ident, op), amt)
    
    semicolon

    return $ Rule (Map.fromList conditions) (Map.fromList deltas)
