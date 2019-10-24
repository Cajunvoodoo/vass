module Data.VASS.Read.Shared where

import Text.Megaparsec
import Data.Void

import Text.Megaparsec.Char (space1)
import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import Control.Monad (void)

--------------------------------------------------------------------------------
-- * TYPE DEFINITIONS

{-| Very basic and common synonym, just to tidy up the types in the parsers. -}
type Parser = Parsec Void String


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
keyword w = lexeme $ symbol_ w

keywords :: [String]
keywords = ["vars", "rules", "init", "target"]

identifier :: Parser String
identifier = lexeme (validIdent >>= check)
    where
        validIdent = (:) <$> identHead <*> many identTail
        identHead = Char.letterChar   <|> oneOf ("_" :: String)
        identTail = Char.alphaNumChar <|> oneOf ("_" :: String)

        check x = if x `elem` keywords
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x
    


semicolon :: Parser String
semicolon = symbol ";"

comma :: Parser String
comma = symbol ","