{-# LANGUAGE CPP #-}

import Control.Monad (foldM_, forM_)
import Data.Maybe ( fromMaybe )
import System.Cmd
import System.Exit
import System.Info (os)
import System.FilePath
import System.Directory ( doesFileExist, copyFile, removeFile, createDirectoryIfMissing )

import Distribution.PackageDescription
import Distribution.MacOSX
import Distribution.Simple.Setup
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
    { postInst = appBundleInstallHook [geniApp DoNotChase]
    , postBuild = appBundleBuildHook  [geniApp DoNotChase]
    }

geniApp :: ChaseDeps -> MacApp
geniApp =
  MacApp "geni"
         (Just "etc/macstuff/wxmac.icns")
         (Just "etc/macstuff/Info.plist")
         []
         []
