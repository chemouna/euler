module Paths_euler_haskell (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mounacheikhna/Documents/repos/mine/euler/euler-haskell/.stack-work/install/x86_64-osx/lts-6.14/7.10.3/bin"
libdir     = "/Users/mounacheikhna/Documents/repos/mine/euler/euler-haskell/.stack-work/install/x86_64-osx/lts-6.14/7.10.3/lib/x86_64-osx-ghc-7.10.3/euler-haskell-0.1.0.0-ElRRL4IsulO5bOLqsUFmzH"
datadir    = "/Users/mounacheikhna/Documents/repos/mine/euler/euler-haskell/.stack-work/install/x86_64-osx/lts-6.14/7.10.3/share/x86_64-osx-ghc-7.10.3/euler-haskell-0.1.0.0"
libexecdir = "/Users/mounacheikhna/Documents/repos/mine/euler/euler-haskell/.stack-work/install/x86_64-osx/lts-6.14/7.10.3/libexec"
sysconfdir = "/Users/mounacheikhna/Documents/repos/mine/euler/euler-haskell/.stack-work/install/x86_64-osx/lts-6.14/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "euler_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "euler_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "euler_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "euler_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "euler_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
