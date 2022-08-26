module Utils where

import Prelude
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder
import Data.Int

whileM :: Monad m => m Bool -> m () -> m ()
whileM cond dothis = do
    yes <- cond
    if yes then dothis >> whileM cond dothis
           else return ()

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond thenM elseM = do
    yes <- cond
    if yes then thenM else elseM

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond thenM = do
    yes <- cond
    if yes then thenM else return ()

slice :: ByteString -> Int64 -> Int64 -> ByteString
slice bs start end = toLazyByteString $
    mconcat $ map (\i -> word8Dec (LBS.index bs i)) [start..end]

panic :: String -> a
panic = error
