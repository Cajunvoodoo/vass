module Data.VASS.Read.MIST where

import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Text.Megaparsec.Char (space1)
import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Maybe
import Control.Monad (void)
import Control.Applicative (liftA2)
import Data.Coerce
import Data.Functor ((<&>))
import Data.Function ((&))

import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec.Debug

import Data.VASS
import Data.Coverability
import Data.VASS.Read.Shared

readMIST :: Parser CovProblem
readMIST = do
    places <- Vector.fromList <$> sectionVars
    rules  <- Vector.fromList <$> sectionRules

    initMap   <- sectionInit
    targetMap <- sectionTarget

    let transitions  = Map.insert "μ" (makeTransitions places rules) Map.empty
        initial      = Configuration "μ" $ makeDense initMap places
        target       = Configuration "μ" $ makeDense targetMap places
        states       = Vector.fromList ["μ"]

    let system = VASS (fromIntegral $ length places) places states transitions
    return $ CovProblem {..}


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
            [setName $ coerce $ "t_" ++ show n | n <- [1..length rules]])

    in addPlaceholderNames transitions



--------------------------------------------------------------------------------
-- * PARSER

sectionVars :: Parser [Name Place]
sectionVars = do
    symbol_ "vars"
    many $ coerce <$> try identifier


sectionRules :: Parser [Rule]
sectionRules = do
    symbol_ "rules"
    many $ try rule

sectionInit :: Parser (Map (Name Place) Integer)
sectionInit = do
    symbol_ "init"
    vals <- pair "=" `sepBy` comma
    return $ Map.fromList vals

sectionTarget :: Parser (Map (Name Place) Integer)
sectionTarget = do
    symbol_ "target"
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

--------------------------------------------------------------------------------
-- * LEXING TOOLS

whitespace :: Parser ()
whitespace = Lexer.space space1 lineComment blockComment
    where
    lineComment  = Lexer.skipLineComment "#"
    blockComment = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

integer :: Parser Integer
integer = lexeme Lexer.decimal

symbol :: String -> Parser String
symbol = Lexer.symbol whitespace

symbol_ :: String -> Parser ()
symbol_ = void . Lexer.symbol whitespace

keyword :: String -> Parser ()
keyword w = lexeme (symbol_ w *> notFollowedBy Char.alphaNumChar)

keywords :: [String]
keywords = ["vars", "rules", "init", "target"]

identifier :: Parser String
identifier = lexeme (validIdent >>= check)
    where
        validIdent = (:) <$> identHead <*> many identTail
        identHead = Char.letterChar
        identTail = Char.alphaNumChar <|> oneOf ("_" :: String)

        check x = if x `elem` keywords
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x
    


semicolon :: Parser String
semicolon = symbol ";"

comma :: Parser String
comma = symbol ","