{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_advent_of_code (
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

bindir     = "/Users/takemyoxygen/dev/code/playground/hs/advent-of-code-2016/.stack-work/install/x86_64-osx/lts-9.12/8.0.2/bin"
libdir     = "/Users/takemyoxygen/dev/code/playground/hs/advent-of-code-2016/.stack-work/install/x86_64-osx/lts-9.12/8.0.2/lib/x86_64-osx-ghc-8.0.2/advent-of-code-0.1.0.0"
dynlibdir  = "/Users/takemyoxygen/dev/code/playground/hs/advent-of-code-2016/.stack-work/install/x86_64-osx/lts-9.12/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/takemyoxygen/dev/code/playground/hs/advent-of-code-2016/.stack-work/install/x86_64-osx/lts-9.12/8.0.2/share/x86_64-osx-ghc-8.0.2/advent-of-code-0.1.0.0"
libexecdir = "/Users/takemyoxygen/dev/code/playground/hs/advent-of-code-2016/.stack-work/install/x86_64-osx/lts-9.12/8.0.2/libexec"
sysconfdir = "/Users/takemyoxygen/dev/code/playground/hs/advent-of-code-2016/.stack-work/install/x86_64-osx/lts-9.12/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "advent_of_code_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "advent_of_code_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "advent_of_code_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "advent_of_code_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "advent_of_code_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "advent_of_code_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
