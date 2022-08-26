module Token where

import Prelude
import Data.Int
import Data.ByteString.Lazy

data TokenType
    -- single char tokens
    = TOKEN_LEFT_PAREN | TOKEN_RIGHT_PAREN
    | TOKEN_LEFT_BRACE | TOKEN_RIGHT_BRACE
    | TOKEN_LEFT_BRACKET | TOKEN_RIGHT_BRACKET
    | TOKEN_COMMA | TOKEN_DOT | TOKEN_MINUS | TOKEN_PLUS
    | TOKEN_SEMICOLON | TOKEN_SLASH | TOKEN_STAR
    | TOKEN_COLON
    -- one or two char tokens
    | TOKEN_BANG | TOKEN_BANG_EQUAL
    | TOKEN_EQUAL | TOKEN_EQUAL_EQUAL
    | TOKEN_GREATER | TOKEN_GREATER_EQUAL
    | TOKEN_LESS | TOKEN_LESS_EQUAL
    -- Literals
    | TOKEN_IDENTIFIER | TOKEN_STRING | TOKEN_NUMBER
    -- Keywords
    | TOKEN_AND | TOKEN_CLASS | TOKEN_ELSE | TOKEN_FALSE
    | TOKEN_FOR | TOKEN_FUN | TOKEN_IF | TOKEN_NIL | TOKEN_OR
    | TOKEN_PRINT | TOKEN_RETURN | TOKEN_SUPER | TOKEN_THIS
    | TOKEN_TRUE | TOKEN_VAR | TOKEN_CONST | TOKEN_WHILE
    | TOKEN_SWITCH | TOKEN_CASE | TOKEN_DEFAULT
    | TOKEN_BREAK | TOKEN_CONTINUE

    | TOKEN_ERROR | TOKEN_EOF
    deriving (Show, Eq, Ord)

data Token = Token {
    src       :: !ByteString
  , tokenType :: !TokenType
  , tokstart  :: !Int64
  , len       :: !Int64
  , loc       :: !Int64 -- ^ Line of code
} deriving (Show, Eq, Ord)
