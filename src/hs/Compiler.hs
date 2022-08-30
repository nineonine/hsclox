{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE TypeApplications         #-}
module Compiler where

import Prelude hiding (error)
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Data.Bits
import Data.ByteString.Lazy hiding (hPutStr)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text hiding (unpack)
import Data.Word
import System.IO (stderr, hPutStr)
import Text.Printf

import Chunk
import Object
import OpCode
import Parser
import Scanner
import Token
import Utils
import Value

-- The bytecode compiler / assembler

foreign export ccall compileFromHs :: IO ()

uINT16_MAX :: Word16
uINT16_MAX = 65535

compileFromHs :: IO ()
compileFromHs = print @String "compiling in hs ..."

type CompilerT = StateT CompilerState IO

data CompilerState = CS {
    scanner      :: !Scanner
  , parser       :: !Parser
  , current      :: !Compiler
  , currentClass :: !ClassCompiler
}

getScanner :: CompilerT Scanner
getScanner = do
    CS{..} <- get
    return scanner

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

getCurrentChunk :: CompilerT Chunk
getCurrentChunk = do
    compiler <- getCurrent
    return compiler.objFunction.chunk

runScanner :: ScannerT a -> CompilerT a
runScanner scanAction = do
    scanner <- getScanner
    (res, scanner') <- liftIO (runStateT scanAction scanner)
    modify' (\st -> st { scanner = scanner'})
    return res

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

errorAt :: Token -> ByteString -> CompilerT ()
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
            liftIO (hPutStr stderr (printf ": %s\n" (LBS8.unpack message)))
            modify' (\st@CS{..} ->
                st { parser = parser { hadError = True }}))

error :: ByteString -> CompilerT ()
error msg = do
    parser <- getParser
    errorAt parser.previousTok msg

errorAtCurrent :: ByteString -> CompilerT ()
errorAtCurrent msg = do
    parser <- getParser
    errorAt parser.currentTok msg

advance :: CompilerT ()
advance = do
    modify' $ \cs -> cs {
      parser = cs.parser {
        previousTok = cs.parser.currentTok
    }}
    go_advance where
      go_advance = do
        t <- runScanner scanToken
        modify' $ \cs -> cs {
          parser = cs.parser {
            currentTok = t
        }}
        parser :: Parser <- getParser
        scanner :: Scanner <- getScanner
        case parser.previousTok.tokenType of
          TOKEN_ERROR -> do
            let tokStr = substr scanner.stream
                                parser.previousTok.tokstart
                                parser.previousTok.len
            errorAtCurrent tokStr
          _else -> go_advance

consume :: TokenType -> ByteString -> CompilerT ()
consume tokenType msg = do
    parser <- getParser
    if parser.currentTok.tokenType == tokenType
    then advance
    else errorAtCurrent msg

check :: TokenType -> CompilerT Bool
check tokenType = do
    parser <- getParser
    return (parser.currentTok.tokenType == tokenType)

match :: TokenType -> CompilerT Bool
match tokenType = do
    ifM (check tokenType)
        (return False)
        (advance >> return True)

emitByte :: Word8 -> CompilerT ()
emitByte byte = do
    chunk <- getCurrentChunk
    parser <- getParser
    modify' $ \cs -> cs {
      current = cs.current {
        objFunction = cs.current.objFunction {
          chunk = writeChunk chunk byte parser.previousTok.loc
        }
      }
    }

emitBytes :: Word8 -> Word8 -> CompilerT ()
emitBytes byte1 byte2 = do
    emitByte byte1
    emitByte byte2

emitLoop :: Int -> CompilerT ()
emitLoop loopStart = do
    emitByte (toBytes OP_LOOP)
    chunk <- getCurrentChunk
    let offset :: Word16 = fromIntegral (chunk.count - loopStart + 2)
    if (offset > uINT16_MAX)
    then error "Loop body too large."
    else do emitByte (fromIntegral $ (offset `shiftR` 8) .&. 0xff)
            emitByte (fromIntegral $ offset .&. 0xff)

emitJump :: Word8 -> CompilerT Int
emitJump  instruction = do
    emitByte instruction
    emitByte 0xff
    emitByte 0xff
    chunk <- getCurrentChunk
    return (chunk.count - 2)

emitReturn :: CompilerT ()
emitReturn = do
    ifM ((TYPE_INITIALIZER ==) . funType <$> getCurrent)
        (emitBytes (toBytes OP_GET_LOCAL) 0)
        (emitByte (toBytes OP_NIL))
    emitByte (toBytes OP_RETURN)

-- TODO: what to do with that constant write? hmm ...
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
addUpvalue compiler idx isLocal = return ()

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
