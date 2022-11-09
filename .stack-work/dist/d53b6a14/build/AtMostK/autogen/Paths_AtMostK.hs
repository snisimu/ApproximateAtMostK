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

bindir     = "D:\\Repos\\App\\AtMostK\\.stack-work\\install\\28cbec53\\bin"
libdir     = "D:\\Repos\\App\\AtMostK\\.stack-work\\install\\28cbec53\\lib\\x86_64-windows-ghc-9.0.2\\AtMostK-0.1.0.0-J1opn9WGkuRGYQf7IHruqM-AtMostK"
dynlibdir  = "D:\\Repos\\App\\AtMostK\\.stack-work\\install\\28cbec53\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "D:\\Repos\\App\\AtMostK\\.stack-work\\install\\28cbec53\\share\\x86_64-windows-ghc-9.0.2\\AtMostK-0.1.0.0"
libexecdir = "D:\\Repos\\App\\AtMostK\\.stack-work\\install\\28cbec53\\libexec\\x86_64-windows-ghc-9.0.2\\AtMostK-0.1.0.0"
sysconfdir = "D:\\Repos\\App\\AtMostK\\.stack-work\\install\\28cbec53\\etc"

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
