#!/usr/bin/env runhaskell

This Cabal setup script is meant to be used with programs that use the
wxHaskell toolkit.  The problem is that on MacOS X, you have to post
process each GUI with the 'macosx-app' shell script (from wxhaskell) so
that it actually responds to user input instead of just sitting there
looking pretty.

> import Control.Monad (foldM_, forM_)
> import Data.Maybe ( fromMaybe )
> import System.Cmd
> import System.Exit
> import System.Info (os)
>
> import Distribution.PackageDescription
> import Distribution.Simple.Setup
> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
>
> import System.FilePath
> import System.Directory ( doesFileExist, copyFile, removeFile, createDirectoryIfMissing )

Configure this stuff
--------------------
Put here the list of executables which contain a GUI.  If they all
contain a GUI (or you don't really care that much), just put Nothing

> mRestrictTo :: Maybe [String]
> mRestrictTo = Just ["geni"]

Put here IO actions needed to add any fancy things (eg icons)
you want to your application bundle.

> customiseAppBundle :: FilePath -- ^ app bundle path
>                    -> FilePath -- ^ full path to original binary
>                    -> IO ()
> customiseAppBundle bundleDir p =
>  case takeFileName p of
>   "geni" ->
>     do hasRez <- doesFileExist "/Developer/Tools/Rez"
>        if hasRez
>           then do -- set the icon
>                   copyFile "etc/macstuff/Info.plist" (bundleDir </> "Contents/Info.plist")
>                   copyFile "etc/macstuff/wxmac.icns" (bundleDir </> "Contents/Resources/wxmac.icns")
>                   -- no idea what this does
>                   system ("/Developer/Tools/Rez -t APPL Carbon.r -o " ++ bundleDir </> "Contents/MacOS/geni")
>                   writeFile (bundleDir </> "PkgInfo") "APPL????"
>                   -- tell Finder about the icon
>                   system ("/Developer/Tools/SetFile -a C " ++ bundleDir </> "Contents")
>                   return ()
>           else putStrLn "Developer Tools not found.  Too bad; no fancy icons for you."
>   ""     -> return ()

Nothing to configure from here on
---------------------------------
Note that we assume anybody running on Darwin is running OS X, probably
not the right thing to do, but frankly... who runs Darwin anyway?  Note
also that this script is equivalent to the vanilla one if you're running
on other operating systems.

> main :: IO ()
> main = defaultMainWithHooks $ addMacHook $ simpleUserHooks { runTests = runTests' }
>  where
>   addMacHook h =
>    case os of
>     "darwin" -> h { postInst = appBundleHook }
>     _        -> h
>

Creating app bundles on installation
------------------------------------

> appBundleHook :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
> appBundleHook _ _ pkg localb =
>  forM_ exes $ \app ->
>    do createAppBundle theBindir (buildDir localb </> app </> app)
>       customiseAppBundle (appBundlePath theBindir app) app
>         `catch` \err -> putStrLn $ "Warning: could not customise bundle for " ++ app ++ ": " ++ show err
>       removeFile (theBindir </> app)
>       createAppBundleWrapper theBindir app
>  where
>   theBindir = bindir $ absoluteInstallDirs pkg localb NoCopyDest
>   exes = fromMaybe (map exeName $ executables pkg) mRestrictTo

> -- | 'createAppBundle' @d p@ - creates an application bundle in @d@
> --   for program @p@, assuming that @d@ already exists and is a directory.
> --   Note that only the filename part of @p@ is used.
> createAppBundle :: FilePath -> FilePath -> IO ()
> createAppBundle dir p =
>  do createDirectoryIfMissing False $ bundle
>     createDirectoryIfMissing True  $ bundleBin
>     createDirectoryIfMissing True  $ bundleRsrc
>     copyFile p (bundleBin </> takeFileName p)
>  where
>   bundle     = appBundlePath dir p
>   bundleBin  = bundle </> "Contents/MacOS"
>   bundleRsrc = bundle </> "Contents/Resources"

> -- | 'createAppBundleWrapper' @d p@ - creates a script in @d@ that calls
> --   @p@ from the application bundle @d </> takeFileName p <.> "app"@
> createAppBundleWrapper :: FilePath -> FilePath -> IO ()
> createAppBundleWrapper bindir p =
>   writeFile (bindir </> takeFileName p) scriptTxt
>  where
>   scriptTxt = "`dirname $0`" </> appBundlePath "." p </> "Contents/MacOS" </> takeFileName p ++ " \"$@\""

> appBundlePath :: FilePath -> FilePath -> FilePath
> appBundlePath dir p = dir </> takeFileName p <.> "app"

Running the test suite
----------------------

> runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
> runTests' _ _ _ lbi = system testprog >> return ()
>  where testprog = (buildDir lbi) </> "geni" </> "geni --unit-tests"
