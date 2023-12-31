{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_contravariant (
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
version = Version [1,5,5] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/cntrvrnt-1.5.5-77fffc68/bin"
libdir     = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/cntrvrnt-1.5.5-77fffc68/lib"
dynlibdir  = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/lib"
datadir    = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/cntrvrnt-1.5.5-77fffc68/share"
libexecdir = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/cntrvrnt-1.5.5-77fffc68/libexec"
sysconfdir = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/cntrvrnt-1.5.5-77fffc68/etc"

getBinDir     = catchIO (getEnv "contravariant_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "contravariant_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "contravariant_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "contravariant_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "contravariant_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "contravariant_sysconfdir") (\_ -> return sysconfdir)




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
