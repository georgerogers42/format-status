module Paths_format_status (
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
version = Version {versionBranch = [0,1,0,2], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/george/.cabal/bin"
libdir     = "/home/george/.cabal/lib/x86_64-linux-ghc-7.4.1/format-status-0.1.0.2"
datadir    = "/home/george/.cabal/share/x86_64-linux-ghc-7.4.1/format-status-0.1.0.2"
libexecdir = "/home/george/.cabal/libexec"
sysconfdir = "/home/george/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "format_status_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "format_status_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "format_status_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "format_status_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "format_status_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
