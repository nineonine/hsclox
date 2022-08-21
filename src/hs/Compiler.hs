module Compiler where

import Control.Monad.Trans.State.Strict
import Data.Map.Strict

import Obj
import Token

-- The bytecode compiler / assembler

data SessionState = SessionState {
    parser   :: !Parser
  , compiler :: !Compiler
}

data Parser = Parser {
    current   :: !Token
  , previous  :: !Token
  , hadError  :: !Bool
  , panicMode :: !Bool
}

type CompilerT = StateT SessionState IO

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

type ParseFn = Bool -> CompilerT ()

data ParseRule = ParseRule {
    prec_prefix :: !ParseFn
  , prec_infix  :: !ParseFn
  , precedence  :: !Precedence
}

data Local = Local {
    name       :: !Token
  , depth      :: !Int
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
    start      :: !Int
  , body       :: !Int
  , scopeDepth :: !Int
  , enclosing  :: !Loop
}

type Globals = Map String Value

data Compiler = Compiler {
    enclosing :: Compiler
  , function :: ObjFunction
  , funType  :: FunctionType
  , locals :: [Local]
  , globals :: Globals
  , localCount :: Int
  , loop :: Loop
}
