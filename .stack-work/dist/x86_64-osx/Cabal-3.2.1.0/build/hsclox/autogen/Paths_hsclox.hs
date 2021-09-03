{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hsclox (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/nineonine/Projects/hsclox/.stack-work/install/x86_64-osx/7007185c7ac768572a1979162793e61522fa3fdaf7296eccfd6d621992615e79/8.10.6/bin"
libdir     = "/Users/nineonine/Projects/hsclox/.stack-work/install/x86_64-osx/7007185c7ac768572a1979162793e61522fa3fdaf7296eccfd6d621992615e79/8.10.6/lib/x86_64-osx-ghc-8.10.6/hsclox-0.1.0.0-H3dTUH0gTZz3CwmTUFmJ2e-hsclox"
dynlibdir  = "/Users/nineonine/Projects/hsclox/.stack-work/install/x86_64-osx/7007185c7ac768572a1979162793e61522fa3fdaf7296eccfd6d621992615e79/8.10.6/lib/x86_64-osx-ghc-8.10.6"
datadir    = "/Users/nineonine/Projects/hsclox/.stack-work/install/x86_64-osx/7007185c7ac768572a1979162793e61522fa3fdaf7296eccfd6d621992615e79/8.10.6/share/x86_64-osx-ghc-8.10.6/hsclox-0.1.0.0"
libexecdir = "/Users/nineonine/Projects/hsclox/.stack-work/install/x86_64-osx/7007185c7ac768572a1979162793e61522fa3fdaf7296eccfd6d621992615e79/8.10.6/libexec/x86_64-osx-ghc-8.10.6/hsclox-0.1.0.0"
sysconfdir = "/Users/nineonine/Projects/hsclox/.stack-work/install/x86_64-osx/7007185c7ac768572a1979162793e61522fa3fdaf7296eccfd6d621992615e79/8.10.6/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hsclox_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hsclox_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hsclox_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hsclox_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hsclox_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hsclox_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
