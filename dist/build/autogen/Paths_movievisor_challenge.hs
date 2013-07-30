module Paths_movievisor_challenge (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/aaron/.cabal/bin"
libdir     = "/home/aaron/.cabal/lib/movievisor-challenge-0.0/ghc-7.4.1"
datadir    = "/home/aaron/.cabal/share/movievisor-challenge-0.0"
libexecdir = "/home/aaron/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "movievisor_challenge_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "movievisor_challenge_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "movievisor_challenge_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "movievisor_challenge_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
