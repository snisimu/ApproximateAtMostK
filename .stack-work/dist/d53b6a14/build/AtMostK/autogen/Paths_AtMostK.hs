{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_AtMostK (
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

bindir     = "C:\\Users\\user\\Repos\\App\\AtMostK\\.stack-work\\install\\d3856219\\bin"
libdir     = "C:\\Users\\user\\Repos\\App\\AtMostK\\.stack-work\\install\\d3856219\\lib\\x86_64-windows-ghc-9.0.2\\AtMostK-0.1.0.0-IJtX3cDeyYTLdFihc8ngGL-AtMostK"
dynlibdir  = "C:\\Users\\user\\Repos\\App\\AtMostK\\.stack-work\\install\\d3856219\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\user\\Repos\\App\\AtMostK\\.stack-work\\install\\d3856219\\share\\x86_64-windows-ghc-9.0.2\\AtMostK-0.1.0.0"
libexecdir = "C:\\Users\\user\\Repos\\App\\AtMostK\\.stack-work\\install\\d3856219\\libexec\\x86_64-windows-ghc-9.0.2\\AtMostK-0.1.0.0"
sysconfdir = "C:\\Users\\user\\Repos\\App\\AtMostK\\.stack-work\\install\\d3856219\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "AtMostK_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "AtMostK_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "AtMostK_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "AtMostK_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AtMostK_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AtMostK_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
