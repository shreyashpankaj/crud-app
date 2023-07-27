{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_assoc (
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
version = Version [1,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/ssc-1.1-2b31584f/bin"
libdir     = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/ssc-1.1-2b31584f/lib"
dynlibdir  = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/lib"
datadir    = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/ssc-1.1-2b31584f/share"
libexecdir = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/ssc-1.1-2b31584f/libexec"
sysconfdir = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/ssc-1.1-2b31584f/etc"

getBinDir     = catchIO (getEnv "assoc_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "assoc_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "assoc_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "assoc_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "assoc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "assoc_sysconfdir") (\_ -> return sysconfdir)




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
