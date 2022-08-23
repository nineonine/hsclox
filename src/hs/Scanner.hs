{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
module Scanner where

import Control.Monad (void)
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Int (Int64)

import Token
import Utils

data Scanner = Scanner {
    stream  :: !ByteString
  , start   :: !Int64
  , current :: !Int64
  , line    :: !Int64
  , len     :: !Int64
} deriving Show

type ScannerT = StateT Scanner IO

initScanner :: ByteString -> ScannerT Scanner
initScanner src = return (Scanner src 0 0 1 (LBS.length src))

isAtEnd :: ScannerT Bool
isAtEnd = do
    Scanner{..} <- get
    return (current >= len)

advance :: ScannerT Char
advance = do
    s@Scanner{..} <- get
    let c = LBS8.index stream current
    put s{current = succ current}
    return c

peek :: ScannerT Char
peek = do
    Scanner{..} <- get
    return (LBS8.index stream current)

peekNext :: ScannerT Char
peekNext =
    ifM isAtEnd
        (return ' ')
        (do Scanner{..} <- get
            return (LBS8.index stream (current + 1)))

match :: Char -> ScannerT Bool
match expected =
    ifM isAtEnd
        (return False)
        (do Scanner{..} <- get
            let c = LBS8.index stream current
            if (c /= expected) then return False
            else do
              modify' (\sc -> sc{current = succ current})
              return True)

makeToken :: TokenType -> ScannerT Token
makeToken tokenType = do
    Scanner{..} <- get
    let l  = current - start
    return (Token stream tokenType start l line)

errorToken :: ByteString -> ScannerT Token
errorToken msg = do
    Scanner{..} <- get
    return (Token msg TOKEN_ERROR 0 (LBS.length msg) line)

number :: ScannerT Token
number = do
    whileM (isDigit <$> peek) (void advance)
    ifM (do c <- peek
            nextC <- peekNext
            return (c == '.' && isDigit nextC))
        (do void advance
            whileM (isDigit <$> peek) (void advance))
        (return ())
    makeToken TOKEN_NUMBER

skipWhiteSpace :: ScannerT ()
skipWhiteSpace = do
    c <- peek
    case c of
        ' '  -> advance >> skipWhiteSpace
        '\r' -> advance >> skipWhiteSpace
        '\t' -> advance >> skipWhiteSpace
        '\n' -> do modify' (\s@Scanner{..} -> s{line = succ line} :: Scanner)
                   advance >> skipWhiteSpace
        '/'  -> ifM (((==) '/') <$> peekNext)
                    (whileM (do c' <- peek
                                isisAtEnd <- isAtEnd
                                return (c' /= '\n' && not isisAtEnd))
                            (void advance))
                    (return ())
        _    -> return ()

checkKeyword :: Int64 -> Int64 -> ByteString -> TokenType
             -> ScannerT TokenType
checkKeyword st ln rest ty = do
    Scanner{..} <- get
    let bs = slice stream (start + st) ((start + st + ln))
    if (current - start == st + ln
            &&  bs == rest)
        then return ty
        else return TOKEN_IDENTIFIER

identifierType :: ScannerT TokenType
identifierType = do
    Scanner{..} <- get
    case LBS8.index stream start of
        'a' -> checkKeyword 1 2 "nd" TOKEN_AND
        'b' -> checkKeyword 1 4 "reak" TOKEN_BREAK
        'c' -> if current - start > 1
            then case (LBS8.index stream (start + 1)) of
                'a' -> checkKeyword 2 2 "se" TOKEN_CASE
                'l' -> checkKeyword 2 3 "ass" TOKEN_CLASS
                'o' -> if current - start > 2 then
                       case (LBS8.index stream (start + 3)) of
                            's' -> checkKeyword 4 1 "t" TOKEN_CONST
                            't' -> checkKeyword 4 4 "inue" TOKEN_CONTINUE
                            _  -> return TOKEN_IDENTIFIER
                       else return TOKEN_IDENTIFIER
                _  -> return TOKEN_IDENTIFIER
            else return TOKEN_IDENTIFIER
        'd' -> checkKeyword 1 6 "efault" TOKEN_DEFAULT
        'e' -> checkKeyword 1 3 "lse" TOKEN_ELSE
        'f' -> if current - start > 1
            then case (LBS8.index stream (start + 1)) of
                'a' -> checkKeyword 2 3 "lse" TOKEN_FALSE
                'o' -> checkKeyword 2 1 "r" TOKEN_FOR
                'u' -> checkKeyword 2 1 "n" TOKEN_FUN
                _   -> return TOKEN_IDENTIFIER
            else return TOKEN_IDENTIFIER
        'i' -> checkKeyword 1 1 "f" TOKEN_IF
        'n' -> checkKeyword 1 2 "il" TOKEN_NIL
        'o' -> checkKeyword 1 1 "r" TOKEN_OR
        'p' -> checkKeyword 1 4 "rint" TOKEN_PRINT
        'r' -> checkKeyword 1 5 "eturn" TOKEN_RETURN
        's' -> if current - start > 1
            then case (LBS8.index stream (start + 1)) of
                'u' -> checkKeyword 2 3 "per" TOKEN_SUPER
                'w' -> checkKeyword 2 4 "itch" TOKEN_SWITCH
                _   -> return TOKEN_IDENTIFIER
            else return TOKEN_IDENTIFIER
        't' -> if current - start > 1
            then case (LBS8.index stream (start + 1)) of
                'h' -> checkKeyword 2 2 "is" TOKEN_THIS
                'r' -> checkKeyword 2 4 "itch" TOKEN_SWITCH
                _  -> return TOKEN_IDENTIFIER
            else return TOKEN_IDENTIFIER
        'v' -> checkKeyword 1 2 "ar" TOKEN_VAR
        'w' -> checkKeyword 1 4 "hile" TOKEN_WHILE
        _   -> return TOKEN_IDENTIFIER


identifier :: ScannerT Token
identifier = do
    whileM (do c <- peek
               return (isAlpha c || isDigit c))
           (void advance)
    identifierType >>= makeToken

string :: ScannerT Token
string = do
    whileM ((&&) <$> (not <$> peekEqualsTo '"') <*> (not <$> isAtEnd))
           (do whenM (peekEqualsTo '\n')
                     (modify' $ \s@Scanner{..} -> s{line = succ line})
               void advance)
    whenM isAtEnd (error "Unterminated string")
    void advance
    makeToken TOKEN_STRING

scanToken :: ScannerT Token
scanToken = do
    skipWhiteSpace
    modify' $ \s@Scanner{..} -> (s {start = current})
    ifM isAtEnd
        (makeToken TOKEN_EOF)
        (do c <- advance
            if | isAlpha c -> identifier
               | isDigit c -> number
               | otherwise -> case c of
                '(' -> makeToken TOKEN_LEFT_PAREN
                ')' -> makeToken TOKEN_RIGHT_PAREN
                '{' -> makeToken TOKEN_LEFT_BRACE
                '}' -> makeToken TOKEN_RIGHT_BRACE
                '[' -> makeToken TOKEN_LEFT_BRACKET
                ']' -> makeToken TOKEN_RIGHT_BRACKET
                ';' -> makeToken TOKEN_SEMICOLON
                ':' -> makeToken TOKEN_COLON
                ',' -> makeToken TOKEN_COMMA
                '.' -> makeToken TOKEN_DOT
                '-' -> makeToken TOKEN_MINUS
                '+' -> makeToken TOKEN_PLUS
                '/' -> makeToken TOKEN_SLASH
                '*' -> makeToken TOKEN_STAR
                '!' -> match '=' >>= \case
                        True -> makeToken TOKEN_BANG_EQUAL
                        False -> makeToken TOKEN_BANG
                '=' -> match '=' >>= \case
                        True -> makeToken TOKEN_EQUAL_EQUAL
                        False -> makeToken TOKEN_EQUAL
                '<' -> match '=' >>= \case
                        True -> makeToken TOKEN_LESS_EQUAL
                        False -> makeToken TOKEN_LESS
                '>' -> match '=' >>= \case
                        True -> makeToken TOKEN_GREATER_EQUAL
                        False -> makeToken TOKEN_GREATER
                '"' -> string
                _   -> errorToken "Unexpected character")

peekEqualsTo :: Char -> ScannerT Bool
peekEqualsTo c = do
    x <- peek
    return (x == c)
