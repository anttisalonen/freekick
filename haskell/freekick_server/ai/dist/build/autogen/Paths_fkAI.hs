module Paths_fkAI (
	version,
	getBinDir, getLibDir, getDataDir, getLibexecDir,
	getDataFileName
	) where

import Data.Version

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/antti/cabaltest/bin"
libdir     = "/home/antti/cabaltest/lib/fkAI-0.0.1/ghc-6.8.2"
datadir    = "/home/antti/cabaltest/share/fkAI-0.0.1"
libexecdir = "/home/antti/cabaltest/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = return bindir
getLibDir = return libdir
getDataDir = return datadir
getLibexecDir = return libexecdir

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return (datadir ++ "/" ++ name)
