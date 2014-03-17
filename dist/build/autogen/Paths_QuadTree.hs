module Paths_QuadTree (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,10,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/kron/.cabal/bin"
libdir     = "/home/kron/.cabal/lib/QuadTree-0.10.0/ghc-7.6.3"
datadir    = "/home/kron/.cabal/share/QuadTree-0.10.0"
libexecdir = "/home/kron/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "QuadTree_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "QuadTree_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "QuadTree_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "QuadTree_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
