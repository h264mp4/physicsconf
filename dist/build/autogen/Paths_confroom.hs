module Paths_confroom (
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
version = Version {versionBranch = [0,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/peng/yesod-practice/phyconf/confroom/.cabal-sandbox/bin"
libdir     = "/home/peng/yesod-practice/phyconf/confroom/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/confroom-0.0.0"
datadir    = "/home/peng/yesod-practice/phyconf/confroom/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/confroom-0.0.0"
libexecdir = "/home/peng/yesod-practice/phyconf/confroom/.cabal-sandbox/libexec"
sysconfdir = "/home/peng/yesod-practice/phyconf/confroom/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "confroom_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "confroom_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "confroom_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "confroom_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "confroom_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
