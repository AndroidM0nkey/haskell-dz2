{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hw_effects (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/sestepanov/.cabal/bin"
libdir     = "/Users/sestepanov/.cabal/lib/x86_64-osx-ghc-9.2.4/hw-effects-0.1.0.0-inplace"
dynlibdir  = "/Users/sestepanov/.cabal/lib/x86_64-osx-ghc-9.2.4"
datadir    = "/Users/sestepanov/.cabal/share/x86_64-osx-ghc-9.2.4/hw-effects-0.1.0.0"
libexecdir = "/Users/sestepanov/.cabal/libexec/x86_64-osx-ghc-9.2.4/hw-effects-0.1.0.0"
sysconfdir = "/Users/sestepanov/.cabal/etc"

getBinDir     = catchIO (getEnv "hw_effects_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hw_effects_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hw_effects_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hw_effects_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw_effects_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw_effects_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
