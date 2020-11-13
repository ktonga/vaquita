{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_vaquita (
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

bindir     = "/home/gaston/.cabal/bin"
libdir     = "/home/gaston/.cabal/lib/x86_64-linux-ghc-8.10.2/vaquita-0.1.0.0-inplace-vaquita"
dynlibdir  = "/home/gaston/.cabal/lib/x86_64-linux-ghc-8.10.2"
datadir    = "/home/gaston/.cabal/share/x86_64-linux-ghc-8.10.2/vaquita-0.1.0.0"
libexecdir = "/home/gaston/.cabal/libexec/x86_64-linux-ghc-8.10.2/vaquita-0.1.0.0"
sysconfdir = "/home/gaston/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "vaquita_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "vaquita_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "vaquita_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "vaquita_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vaquita_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vaquita_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
