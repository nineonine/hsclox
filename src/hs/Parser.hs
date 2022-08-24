module Parser where

import Data.Int

import Token

data Parser = Parser {
    current   :: !Token
  , previous  :: !Token
  , hadError  :: !Bool
  , panicMode :: !Bool
}

data Precedence
    = PREC_NONE
    | PREC_ASSIGNMENT --  =
    | PREC_OR -- or
    | PREC_AND -- and
    | PREC_EQUALITY -- ==
    | PREC_COMPARISON -- < > <= >=
    | PREC_TERM --  + -
    | PREC_FACTOR--  * /
    | PREC_UNARY -- ! -
    | PREC_CALL --  . ()
    | PREC_PRIMARY

data Local = Local {
    name       :: !Token
  , depth      :: !Int64
  , isCaptured :: !Bool
  , isConst    :: !Bool
}

data Upvalue = Upvalue {
    index :: Int
  , isLocal :: Bool
}

data FunctionType
    = TYPE_FUNCTION
    | TYPE_INITIALIZER
    | TYPE_METHOD
    | TYPE_SCRIPT

data Loop = Loop {
    start      :: !Int64
  , body       :: !Int64
  , scopeDepth :: !Int64
  , enclosing  :: !Loop
}
