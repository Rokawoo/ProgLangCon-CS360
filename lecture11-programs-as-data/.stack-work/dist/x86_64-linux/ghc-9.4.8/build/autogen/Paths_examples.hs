{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_examples (
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
version = Version [0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/workspaces/lectures/lecture11-programs-as-data/.stack-work/install/x86_64-linux/26db9ca6e153da2ac41d8ee4b31b44e1fd0966e530403d5c06178b45d70f31d7/9.4.8/bin"
libdir     = "/workspaces/lectures/lecture11-programs-as-data/.stack-work/install/x86_64-linux/26db9ca6e153da2ac41d8ee4b31b44e1fd0966e530403d5c06178b45d70f31d7/9.4.8/lib/x86_64-linux-ghc-9.4.8/examples-0.1-27ElsXIH93e6fsDJHxumQC"
dynlibdir  = "/workspaces/lectures/lecture11-programs-as-data/.stack-work/install/x86_64-linux/26db9ca6e153da2ac41d8ee4b31b44e1fd0966e530403d5c06178b45d70f31d7/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/workspaces/lectures/lecture11-programs-as-data/.stack-work/install/x86_64-linux/26db9ca6e153da2ac41d8ee4b31b44e1fd0966e530403d5c06178b45d70f31d7/9.4.8/share/x86_64-linux-ghc-9.4.8/examples-0.1"
libexecdir = "/workspaces/lectures/lecture11-programs-as-data/.stack-work/install/x86_64-linux/26db9ca6e153da2ac41d8ee4b31b44e1fd0966e530403d5c06178b45d70f31d7/9.4.8/libexec/x86_64-linux-ghc-9.4.8/examples-0.1"
sysconfdir = "/workspaces/lectures/lecture11-programs-as-data/.stack-work/install/x86_64-linux/26db9ca6e153da2ac41d8ee4b31b44e1fd0966e530403d5c06178b45d70f31d7/9.4.8/etc"

getBinDir     = catchIO (getEnv "examples_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "examples_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "examples_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "examples_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "examples_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "examples_sysconfdir") (\_ -> return sysconfdir)




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
