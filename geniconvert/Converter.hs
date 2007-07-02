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

import Data.Binary (encodeFile)
import Data.IORef (newIORef, modifyIORef, readIORef)
import Data.List (intersperse)
import Data.Maybe
import System (ExitCode(ExitFailure), exitWith, getArgs, getProgName)
import System.Console.GetOpt(OptDescr(Option), ArgDescr(..), usageInfo, getOpt, ArgOrder(Permute))
import System.IO(getContents)
import System.IO.Unsafe(unsafeInterleaveIO)
import Text.ParserCombinators.Parsec

import NLP.GenI.Btypes (Macros,pfamily,MTtree)
import NLP.GenI.BtypesBinary ()
import NLP.GenI.General
import NLP.GenI.GeniParsers (geniMacros)
import NLP.GenI.GeniShow (geniShow)
import NLP.GenI.HsShow (hsShow)
import NLP.GenI.Converter.ReadTagml (readTagmlMacros)

data Flag = FromFlg String | ToFlg String | OutputFlg String
          | MacrosFlg | LexiconFlg | MorphLexiconFlg
 deriving (Eq)

options :: [OptDescr Flag]
options =
  [ Option "f" ["from"] (ReqArg FromFlg "TYPE") "tagml|geni"
  , Option "t" ["to"]   (ReqArg ToFlg "TYPE")   "haskell|geni|genib"
  , Option "o" ["output"]  (ReqArg OutputFlg "STRING")  "output file, or -t haskell, prefix for output files"
  --
  , Option "m" ["--macros"]  (NoArg MacrosFlg)"input file is a macros file (default)"
  , Option "l" ["--lexicon"] (NoArg LexiconFlg) "input file is a lexicon file"
  , Option ""  ["--morphlexicon"] (NoArg MorphLexiconFlg) "input file is a morphological lexicon"
  ]

type Mutex = ([Flag],String)

mutexes :: [Mutex]
mutexes =
  [ ( [MacrosFlg, LexiconFlg, MorphLexiconFlg], "--macros, --lexicon and --morphlexicon")
  ]

-- | Return all failures mutually exclusive options
getMutexFailures :: [Flag] -> [String]
getMutexFailures fs = mapMaybe getMf mutexes
 where
  getMf (mfs,d) = if length [ f | f <- fs, f `elem` mfs ] > 1
                     then Nothing
                     else Just d

hasMutexFailures :: [Flag] -> Bool
hasMutexFailures = not . null . getMutexFailures

data InputType = MacrosItype | LexiconItype | MorphLexiconItype

data InputParams = InputParams { fromArg :: String
                               , toArg   :: String
                               , stemArg :: String
                               , itype   :: InputType
                               }

defaultParams :: InputParams
defaultParams = InputParams "" "" "" MacrosItype

toInputParams :: [Flag] -> InputParams
toInputParams fs = foldr ($) defaultParams $ map processFlag fs

processFlag :: Flag -> InputParams -> InputParams
processFlag (FromFlg x)     = \p -> p { fromArg = x }
processFlag (ToFlg x)       = \p -> p { toArg = x }
processFlag (OutputFlg x)   = \p -> p { stemArg = x }
processFlag MacrosFlg       = \p -> p { itype = MacrosItype }
processFlag LexiconFlg      = \p -> p { itype = LexiconItype }
processFlag MorphLexiconFlg = \p -> p { itype = MorphLexiconItype }

main :: IO ()
main =
 do args <- getArgs
    progname <- getProgName
    case getOpt Permute options args of
     (o, _, _) | hasMutexFailures o ->
       do ePutStr $ unlines [ d ++ " are mutually exclusive" | d <- getMutexFailures o ]
          exitWith (ExitFailure 1)
     (o,fs,[]  ) ->
       let (InputParams iFormat oFormat f iTy) = toInputParams o in
       do mInitialiseFile f
          case oFormat of
           "haskell" -> if null f
                        then ePutStrLn $ "Can't write haskell to stdout (Please provide a stem)."
                        else doHaskell iFormat f fs
           "geni"    -> readAndWriteMacros iFormat fs (geniWriter f)
           "genib"   -> readAndWriteMacros iFormat fs (encodeFile f)
           _         -> do ePutStrLn $ "Unkwown output format: " ++ oFormat
                           exitWith (ExitFailure 1)
     _         -> showUsage progname
 where
  mInitialiseFile "" = return ()
  mInitialiseFile f  = writeFile f ""
  showUsage p =
    do let header = "usage: " ++ p ++ " -f [tagml|geni] -t [haskell|geni|genib] < input > output"
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
everyN _ [] = []
everyN n xs = take n xs : (everyN n $ drop n xs)

uncommas :: [String] -> String
uncommas = concat . (intersperse ", ")
