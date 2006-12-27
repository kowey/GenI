{-
GenI surface realiser
Copyright (C) 2005 Carlos Areces and Eric Kow

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
-}

-- This module is meant to act as a standalone program which serves
-- strictly as a converter between the formats recognised by GenI.

module Main (main) where

import Data.IORef (newIORef, modifyIORef, readIORef)
import Data.List (intersperse)
import System (ExitCode(ExitFailure), exitWith, getArgs, getProgName)
import System.Console.GetOpt(OptDescr(Option), ArgDescr(ReqArg), usageInfo, getOpt, ArgOrder(Permute))
import System.IO(getContents)
import System.IO.Unsafe(unsafeInterleaveIO)
import Text.ParserCombinators.Parsec

import NLP.GenI.Btypes (Macros,pfamily,MTtree)
import NLP.GenI.General (ePutStrLn, toUpperHead)
import NLP.GenI.GeniParsers (geniMacros)
import NLP.GenI.Treeprint (geniShow)
import NLP.GenI.HsShowable (hsShow)
import NLP.GenI.Converter.ReadTagml (readTagmlMacros)

data Flag = FromFlg String | ToFlg String | OutputFlg String

options :: [OptDescr Flag]
options =
  [ Option "f" ["from"] (ReqArg FromFlg "TYPE") "tagml|geni"
  , Option "t" ["to"]   (ReqArg ToFlg "TYPE")   "haskell|geni"
  , Option "o" ["output"]  (ReqArg OutputFlg "STRING")  "output file, or -t haskell, prefix for output files"
  ]

data InputParams = InputParams { fromArg :: String
                               , toArg   :: String
                               , stemArg :: String }

toInputParams :: [Flag] -> InputParams
toInputParams [] = InputParams "" "" ""
toInputParams (FromFlg x : n)    = (toInputParams n) { fromArg = x }
toInputParams (ToFlg x : n)      = (toInputParams n) { toArg = x }
toInputParams (OutputFlg x : n)  = (toInputParams n) { stemArg = x }

main :: IO ()
main =
 do args <- getArgs
    progname <- getProgName
    case getOpt Permute options args of
     (o,fs,[]  ) ->
       let (InputParams fTy tTy f) = toInputParams o in
       do mInitialiseFile f
          case tTy of
           "haskell" -> if null f
                        then ePutStrLn $ "Can't write haskell to stdout (Please provide a stem)."
                        else doHaskell fTy f fs
           "geni"    -> readAndWriteMacros fTy fs (geniWriter f)
           _         -> do ePutStrLn $ "Unkwown output type : " ++ tTy
                           exitWith (ExitFailure 1)
     _         -> showUsage progname
 where
  mInitialiseFile "" = return ()
  mInitialiseFile f  = writeFile f ""
  showUsage p =
    do let header = "usage: " ++ p ++ " -f [tagml|geni] -t [haskell|geni] < input > output"
       ePutStrLn $ usageInfo header options
       exitWith (ExitFailure 1)
  doHaskell fromType f fs =
   do mref <- newIORef []
      readAndWriteMacros fromType fs (\m -> modifyIORef mref (++ m))
      macros <- readIORef mref
      writeHaskell f macros

readAndWriteMacros :: String -> [FilePath] -> (Macros -> IO ()) -> IO ()
readAndWriteMacros f fs writer =
 let reader = case f of
             "tagml" -> tagmlReader
             "geni"  -> geniReader
             _       -> fail ("Unknown -f type: " ++ f)
 in if null fs then getContents >>= reader >>= writer
    else do mapM (\x -> unsafeInterleaveIO (readFile x) >>= reader >>= writer) fs
            return ()

tagmlReader :: String -> IO Macros
tagmlReader lf =
  case readTagmlMacros lf of
  Left err -> fail err
  Right  c -> return c

geniReader :: String -> IO Macros
geniReader lf =
  case parse geniMacros "" lf of
  Left err -> fail (show err)
  Right  c -> return c

geniWriter :: FilePath -> Macros -> IO ()
geniWriter mf ms =
 write $ unlines $ map geniShow ms
 where write = if null mf then putStrLn else appendFile mf

writeHaskell :: String -> Macros -> IO ()
writeHaskell rawStem ms =
 do let tpairs = zip [1::Integer ..] ms
        stem   = toUpperHead rawStem
    -- write the sub files
    let chunks   = everyN 15 tpairs
        chunkIds = take (length chunks) [1::Int ..]
    sequence $ zipWith (writeChunk stem) chunkIds chunks
    -- write the main file
    writeFile (stem ++ ".hs") $ unlines $
      [ "module " ++ stem ++ "(myGeniGrammar) where"
      , "import NLP.GenI.Btypes"
      , "" ] ++
      map (\i -> "import " ++ stem ++ (show i)) chunkIds ++
      [ ""
      , "myGeniGrammar :: [MTtree]"
      , "myGeniGrammar = "
      , " [" ++ (uncommas $ map valName tpairs)
      , " ]" ]

writeChunk :: String -> Int -> [(Integer,MTtree)] -> IO ()
writeChunk stem n tps =
 let filename   = stem ++ (show n) ++ ".hs"
     modulename = stem ++ (show n)
     treenames  = uncommas $ map valName tps
 in  writeFile filename $
     unlines $ [ "module " ++ modulename ++ "(" ++ treenames ++ ") where"
               , "import Data.Tree"
               , "import NLP.GenI.Btypes"
               , ""
               , treenames ++ " :: MTtree"
               , "" ] ++
               (intersperse "" $ map (\ (i,t) -> valName (i,t) ++ " = " ++ hsShow t) tps)

valName :: (Integer, MTtree) -> String
valName (i,t) = "t" ++ (show i) ++ "_" ++ (pfamily t)

-- | Break a list up in to n sized chunks; the last element of the list might
--   be somewhat smaller, eh?
everyN :: Int -> [a] -> [[a]]
everyN n [] = []
everyN n xs = take n xs : (everyN n $ drop n xs)

uncommas :: [String] -> String
uncommas = concat . (intersperse ", ")
