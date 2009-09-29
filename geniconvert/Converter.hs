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

import Control.Monad
import Data.Binary (decodeFile, encodeFile)
import Data.List
import Data.Maybe
import System (ExitCode(ExitFailure), exitWith, getArgs, getProgName)
import System.Console.GetOpt(OptDescr(Option), ArgDescr(..), usageInfo, getOpt, ArgOrder(Permute))
import System.IO(getContents)
import System.IO.Unsafe(unsafeInterleaveIO)
import Text.ParserCombinators.Parsec

import NLP.GenI.Btypes
import NLP.GenI.BtypesBinary ()
import NLP.GenI.General
import NLP.GenI.GeniParsers
import NLP.GenI.GeniShow
import NLP.GenI.HsShow (hsShow)
import NLP.GenI.Converter.ReadTagml (readTagmlMacros)

main :: IO ()
main =
 do args <- getArgs
    progname <- getProgName
    case getOpt Permute options args of
     (o,fs,[]  ) -> convert (toInputParams o) fs
     _           -> showUsage progname
 where
  showUsage p = do
    let header = "usage: " ++ p ++ " [--macros|--lexicon] -f [tagml|geni] -t [haskell|geni|genib] -o output < input"
    ePutStrLn $ usageInfo header options
    exitWith (ExitFailure 1)

convert :: InputParams -> [FilePath] -> IO ()
convert (InputParams iForm oForm f iType) fs =
 do iFormat <- case getFormat iForm of
                 Nothing -> fail $ "Unknown input format: " ++ iForm
                 Just fo -> return fo
    oFormat <- case getFormat oForm of
                 Nothing -> fail $ "Unknown output format: " ++ oForm
                 Just fo -> return fo
    when (null f) $
      fail $ "Sorry, you must specify an output file with -o"
    when (oFormat `elem` [ haskellFormat, genibFormat ] && length fs > 1) $
      fail $ "Sorry, I can't convert more than one file to " ++ showFormat oFormat ++ " at a time"
    -- empty out the file first! (there might be an old one lying around)
    writeFile f ""
    --
    let getParser p = maybe     (oops iFormat "parse") textReader $ p iFormat
        getReader r = fromMaybe (oops iFormat "read")             $ r iFormat
        getWriter w = fromMaybe (oops oFormat "write")            $ w oFormat
        oops format d =
           fail $ unwords [ "Sorry, I don't know how to", d, show iType, "in the", showFormat format, "format" ]
    let convertString x = case iType of
                            MacrosItype       -> getParser parseMacros x       >>= (getWriter writeMacros f)
                            LexiconItype      -> getParser parseLexicon x      >>= (getWriter writeLexicon f)
        convertFile x = case iType of
                            MacrosItype       -> getReader readMacros x       >>= (getWriter writeMacros f)
                            LexiconItype      -> getReader readLexicon x      >>= (getWriter writeLexicon f)
    if null fs
       then getContents >>= convertString
       else forM_ fs $ (\x -> unsafeInterleaveIO (readFile x) >>= convertFile)

-- -------------------------------------------------------------------
-- command line arguments
-- -------------------------------------------------------------------

data Flag = FromFlg String | ToFlg String | OutputFlg String
          | MacrosFlg | LexiconFlg
 deriving (Eq)

options :: [OptDescr Flag]
options =
  [ Option "f" ["from"] (ReqArg FromFlg "TYPE") "tagml|geni"
  , Option "t" ["to"]   (ReqArg ToFlg "TYPE")   "haskell|geni|genib"
  , Option "o" ["output"]  (ReqArg OutputFlg "STRING")  "output file, or -t haskell, prefix for output files"
  --
  , Option ""  ["macros"]  (NoArg MacrosFlg)"input file is a macros file (default)"
  , Option ""  ["lexicon"] (NoArg LexiconFlg) "input file is a lexicon file"
  ]

data InputType = MacrosItype | LexiconItype

instance Show InputType where
  show MacrosItype       = "macros"
  show LexiconItype      = "lexicons"

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

-- -------------------------------------------------------------------
-- formats
-- -------------------------------------------------------------------

type TextParser a = String -> Either String a
type FileReader a = FilePath -> IO a
type FileWriter a = FilePath -> a -> IO ()

data FileFormat = FileFormat
      { showFormat   :: String
      --
      , parseMacros  :: Maybe (TextParser Macros)
      , readMacros   :: Maybe (FileReader Macros)
      , writeMacros  :: Maybe (FileWriter Macros)
      --
      , parseLexicon :: Maybe (TextParser [ILexEntry])
      , readLexicon  :: Maybe (FileReader [ILexEntry])
      , writeLexicon :: Maybe (FileWriter [ILexEntry])
      }

instance Eq FileFormat where
  a == b = showFormat a == showFormat b

wrapParsec :: CharParser () a -> (String -> Either String a)
wrapParsec p lf = either (Left . show) (Right) (parse p "" lf)

textReader :: (String -> Either String a) -> String -> IO a
textReader p lf = either fail return (p lf)

getFormat :: String -> Maybe FileFormat
getFormat x = listToMaybe [ f | f <- formats, x == showFormat f ]

formats :: [FileFormat]
formats = [ tagmlFormat, geniFormat, genibFormat, haskellFormat ]

-- -------------------------------------------------------------------
-- tagml format
-- -------------------------------------------------------------------

tagmlFormat :: FileFormat
tagmlFormat = FileFormat
  { showFormat   = "tagml"
  , parseMacros  = pMacros
  , readMacros   = fmap textReader pMacros
  , writeMacros  = Nothing
  --
  , parseLexicon = pLex
  , readLexicon  = fmap textReader pLex
  , writeLexicon = Nothing
  }
 where
  pMacros   = Just $ readTagmlMacros
  pLex      = Nothing

-- -------------------------------------------------------------------
-- geni format
-- -------------------------------------------------------------------

geniFormat :: FileFormat
geniFormat = FileFormat
  { showFormat   = "geni"
  --
  , parseMacros  = pMacros
  , readMacros   = fmap textReader pMacros
  , writeMacros  = Just geniWriter
  --
  , parseLexicon = pLex
  , readLexicon  = fmap textReader pLex
  , writeLexicon = Nothing
  }
 where
  pMacros   = Just $ wrapParsec geniMacros
  pLex      = Just $ wrapParsec geniLexicon

geniWriter :: GeniShow a => FilePath -> [a] -> IO ()
geniWriter mf ms = appendFile mf $ unlines $ map geniShow ms

-- -------------------------------------------------------------------
-- genib format
-- -------------------------------------------------------------------

genibFormat :: FileFormat
genibFormat = FileFormat
  { showFormat   = "genib"
  --
  , parseMacros  = Nothing
  , readMacros   = Just decodeFile
  , writeMacros  = Just encodeFile
  --
  , parseLexicon = Nothing
  , readLexicon  = Just decodeFile
  , writeLexicon = Just encodeFile
  }

-- -------------------------------------------------------------------
-- haskell macros file
-- -------------------------------------------------------------------

haskellFormat :: FileFormat
haskellFormat = FileFormat
  { showFormat   = "haskell"
  --
  , parseMacros  = Nothing
  , readMacros   = Nothing
  , writeMacros  = Just writeHaskellMacros
  --
  , parseLexicon = Nothing
  , readLexicon  = Nothing
  , writeLexicon = Nothing
  }

writeHaskellMacros :: String -> Macros -> IO ()
writeHaskellMacros rawStem ms =
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
