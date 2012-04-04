module Paths_pancake (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/fox/Library/Haskell/ghc-7.0.4/lib/pancake-0.1/bin"
libdir     = "/Users/fox/Library/Haskell/ghc-7.0.4/lib/pancake-0.1/lib"
datadir    = "/Users/fox/Library/Haskell/ghc-7.0.4/lib/pancake-0.1/share"
libexecdir = "/Users/fox/Library/Haskell/ghc-7.0.4/lib/pancake-0.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "pancake_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "pancake_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "pancake_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "pancake_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
