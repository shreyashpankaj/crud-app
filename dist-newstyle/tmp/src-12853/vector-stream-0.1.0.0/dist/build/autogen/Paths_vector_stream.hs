{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_vector_stream (
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
bindir     = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/vctr-strm-0.1.0.0-af86f344/bin"
libdir     = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/vctr-strm-0.1.0.0-af86f344/lib"
dynlibdir  = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/lib"
datadir    = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/vctr-strm-0.1.0.0-af86f344/share"
libexecdir = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/vctr-strm-0.1.0.0-af86f344/libexec"
sysconfdir = "/Users/shreyash.pankaj/.cabal/store/ghc-8.8.4/vctr-strm-0.1.0.0-af86f344/etc"

getBinDir     = catchIO (getEnv "vector_stream_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "vector_stream_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "vector_stream_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "vector_stream_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vector_stream_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vector_stream_sysconfdir") (\_ -> return sysconfdir)




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
