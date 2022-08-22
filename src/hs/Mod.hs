{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Mod where

foreign export ccall compileFromHs :: IO ()

compileFromHs :: IO ()
compileFromHs = print "compiling in hs ..."
