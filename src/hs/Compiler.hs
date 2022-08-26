{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Compiler where

import Data.Int
import Data.Word
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text
import System.IO (stderr, hPutStr)
import Text.Printf

import Object
import OpCode
import Parser
import Token
import Utils
import Value

-- The bytecode compiler / assembler

foreign export ccall compileFromHs :: IO ()

compileFromHs :: IO ()
compileFromHs = print "compiling in hs ..."

type CompilerT = StateT CompilerState IO

data CompilerState = CS {
    parser       :: !Parser
  , current      :: !Compiler
  , currentClass :: !ClassCompiler
}

getParser :: CompilerT Parser
getParser = do
    CS{..} <- get
    return parser

getCurrent :: CompilerT Compiler
getCurrent = do
    CS{..} <- get
    return current

getCurrentClass :: CompilerT ClassCompiler
getCurrentClass = do
    CS{..} <- get
    return currentClass

type ParseFn = Bool -> CompilerT ()

type Globals = Map String () -- Value

data Compiler = Compiler {
    enclosing :: Compiler
  , objFunction :: ObjFunction
  , funType  :: FunctionType
  , locals :: [Local]
  , globals :: Globals
  , localCount :: Int64
  , loop :: Loop
}

data ClassCompiler = ClassCompiler {
    hasSuperClass :: !Bool
  , enclosing     :: !ClassCompiler
}

data CanAssign = CanAssign | CanNotAssign

currentChunk :: CompilerT Chunk
currentChunk  = do
    CS{..} <- get
    return current.objFunction.chunk

errorAt :: Token -> Text -> CompilerT ()
errorAt Token{..} message = do
    ifM (panicMode <$> getParser)
        (return ())
        (do modify' (\st@CS{..} ->
                st { parser = parser { panicMode = True}})
            let msg = printf "[line %d] Error" loc
            liftIO (hPutStr stderr msg)
            case tokenType of
              TOKEN_EOF -> liftIO (hPutStr stderr " at end")
              TOKEN_ERROR -> return () -- nothing
              _otherwise ->
                let msg1 = printf " at '%.*s'" len tokstart
                in liftIO (hPutStr stderr msg1)
            liftIO (hPutStr stderr (printf ": %s\n" message))
            modify' (\st@CS{..} ->
                st { parser = parser { hadError = True }}))

error :: Text -> CompilerT ()
error msg = do
    parser <- getParser
    errorAt parser.previousTok msg


errorAtCurrent :: Text -> CompilerT ()
errorAtCurrent msg = do
    parser <- getParser
    errorAt parser.currentTok msg

advance :: CompilerT ()
advance = return ()

consume :: TokenType -> Text -> CompilerT ()
consume tokenType msg = return ()

check :: TokenType -> CompilerT Bool
check tokenType = return False

match :: TokenType -> CompilerT ()
match tokenType = return ()

emitByte :: Word8 -> CompilerT ()
emitByte byte = return ()

emitBytes :: Word8 -> Word8 -> CompilerT ()
emitBytes byte1 byte2 = return ()

emitLoop :: Int -> CompilerT ()
emitLoop loopStart = return ()

emitJump :: Word8 -> CompilerT Int
emitJump  instruction = return 1

emitReturn :: CompilerT ()
emitReturn = return ()

makeConstant :: Value -> CompilerT Word8
makeConstant value = return 1

emitConstant :: Value -> CompilerT ()
emitConstant value = do
  c <- makeConstant value
  emitBytes (toBytes OP_CONSTANT) c

patchJump :: Int -> CompilerT ()
patchJump offset = return ()

computeArgsByteSize :: Word8 -> Int -> CompilerT Int
computeArgsByteSize code ip = return 0

startLoop :: Loop -> CompilerT ()
startLoop loop = return ()

endLoop :: CompilerT ()
endLoop = return ()

initCompiler :: Compiler -> FunctionType -> CompilerT ()
initCompiler compiler funType = return ()

endCompiler :: CompilerT ObjFunction
endCompiler = panic "endCompiler"

beginScope :: CompilerT ()
beginScope = return ()

endScope :: CompilerT ()
endScope = return ()

identifierConstant :: Token -> CompilerT Word8
identifierConstant name = return 1

identifiersEqual :: Token -> Token -> CompilerT Bool
identifiersEqual a b = return False

resolveLocal :: Compiler -> Token -> CompilerT Int
resolveLocal compiler name = return 1

addUpvalue :: Compiler -> Word8 -> Bool -> CompilerT ()
addUpvalue compiler index isLocal = return ()

resolveUpvalue :: Compiler -> Token -> CompilerT Int
resolveUpvalue compiler name = return 0

addLocal :: Token -> Bool -> CompilerT ()
addLocal name isConst = return ()

discardLocals :: Compiler -> CompilerT Int
discardLocals current = return 0

declareVariable :: Bool -> CompilerT ()
declareVariable isConst = return ()

grouping :: CanAssign -> CompilerT ()
grouping canAssign = return ()

number :: CanAssign -> CompilerT ()
number canAssign = return ()

or_ :: CanAssign -> CompilerT ()
or_ canAssign = return ()

string :: CanAssign -> CompilerT ()
string canAssign = return ()

isNotConstVar :: Compiler -> Token -> Int -> CompilerT ()
isNotConstVar compiler name i = return ()

namedVariable :: Token -> CanAssign -> CompilerT ()
namedVariable name canAssign = return ()

variable :: CanAssign -> CompilerT ()
variable  canAssign = return ()

syntheticToken :: Text -> Token
syntheticToken = panic "syntheticToken"

argumentList :: CompilerT Word8
argumentList = return 1

super :: CanAssign -> CompilerT ()
super canAssign = return ()

this :: CanAssign -> CompilerT ()
this canAssign = return ()

unary :: CanAssign -> CompilerT ()
unary canAssign = return ()

extendGlobals :: ObjString -> Bool -> CompilerT ()
extendGlobals key isConst = return ()

parseVariable :: Text -> CompilerT Word8
parseVariable errorMessage = return 0

markInitialized :: CompilerT ()
markInitialized = return ()

defineVariable :: Word8 -> CompilerT ()
defineVariable global = return ()

and :: CanAssign -> CompilerT ()
and canAssign = return ()

binary :: CanAssign -> CompilerT ()
binary canAssign = return ()

call :: CanAssign -> CompilerT ()
call canAssign = return ()

field :: CanAssign -> CompilerT ()
field canAssign = return ()

dot :: CanAssign -> CompilerT ()
dot canAssign = return ()

literal :: CanAssign -> CompilerT ()
literal canAssign = return ()

data ParseRule = ParseRule {
    parsingFn :: Maybe (CanAssign -> CompilerT ())
  , parseOpFn :: Maybe (CanAssign -> CompilerT ())
  , precedence :: Precedence
}

rules :: Map TokenType ParseRule
rules = Map.empty

parsePrecedence :: Precedence -> CompilerT ()
parsePrecedence precedence = return ()

getRule :: TokenType -> Maybe ParseRule
getRule tokenType = Map.lookup tokenType rules

expression :: CompilerT ()
expression = return ()

block :: CompilerT ()
block = return ()

function :: FunctionType -> CompilerT ()
function functionType = return ()

method :: CompilerT ()
method = return ()

classDeclaration :: CompilerT ()
classDeclaration = return ()

funDeclaration :: CompilerT ()
funDeclaration = return ()

expressionStatement :: CompilerT ()
expressionStatement = return ()

forStatement :: CompilerT ()
forStatement = return ()

ifStatement :: CompilerT ()
ifStatement = return ()

switchCase :: CompilerT Int
switchCase = return 1

defaultCase :: CompilerT ()
defaultCase = return ()

switchStatement :: CompilerT ()
switchStatement = return ()

printStatement :: CompilerT ()
printStatement = return ()

returnStatement :: CompilerT ()
returnStatement = return ()

breakStatement :: CompilerT ()
breakStatement = return ()

continueStatement :: CompilerT ()
continueStatement = return ()

whileStatement :: CompilerT ()
whileStatement = return ()

synchronize :: CompilerT ()
synchronize = return ()

declaration :: CompilerT ()
declaration = return ()

statement :: CompilerT ()
statement = return ()

-- FFI export ...
compile :: Text -> ObjFunction
compile source = panic "compile"

markCompilerRoots :: CompilerT ()
markCompilerRoots = return ()
