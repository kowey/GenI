#!/usr/bin/env runhaskell

This Cabal setup script is meant to be used with programs that use the
wxHaskell toolkit.  The problem is that on MacOS X, you have to post
process each GUI with the 'macosx-app' shell script (from wxhaskell) so
that it actually responds to user input instead of just sitting there
looking pretty.

> import Control.Monad (foldM_)
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

Configure this stuff
--------------------
Put here the list of executables which contain a GUI.  If they all
contain a GUI (or you don't really care that much), just put Nothing

> mRestrictTo :: Maybe [String]
> mRestrictTo = Just ["geni"]

Normally, this should just be "macosx-app"

> macosxApp :: String
> macosxApp = "etc/macstuff/macosx-app"

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
>     "darwin" -> h { postBuild = macifyHook }
>     _        -> h
>

> macifyHook _ _ pkg localb =
>   foldM_ (next $ macify.binPath) ExitSuccess guiExes
>  where
>   allExes = map exeName $ executables pkg
>   guiExes = case mRestrictTo of
>               Nothing -> allExes
>               Just rs -> filter (`elem` rs) allExes
>   next _ x@(ExitFailure _) _ = return x
>   next _ _ b = macify (binPath b)
>   binPath x = buildDir localb </> x </> x

> macify :: FilePath -> IO ExitCode
> macify x = system $ unwords $ [ "chmod u+x",  macosxApp,  ";"
>                               ,  macosxApp, x ]

Running the test suite
----------------------

> runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
> runTests' _ _ _ lbi = system testprog >> return ()
>  where testprog = (buildDir lbi) </> "geni" </> "geni --unit-tests"
