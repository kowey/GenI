GPL License
===========
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

About this program
==================
Just think of this as a throwaway script.  The purpose is to read a
semsuite where each test case is annotated with the actual outputs
produced by GenI.  (The annotations are from the output: keyword).
We output a gnuplot plot file : the X axis contains a distinct number of
paraphrases, the Y axis contains the number of test cases with that many
paraphrases.

What we really need is a libgeni.

> module Main (main) where
>
> import NLP.GenI.Btypes (tcOutputs)
> import NLP.GenI.General (ePutStrLn)
> import NLP.GenI.GeniParsers(geniTestSuite)
>
> import Data.List (minimum, maximum, partition, intersperse)
> import Data.Maybe (fromMaybe)
> import System.Console.GetOpt
> import System.Environment
> import System.Exit(exitFailure)
> import System.IO
> import Text.ParserCombinators.Parsec


> main :: IO ()
> main =
>  do settings <- readArgv
>     let Settings { s_suiteFile = sFile
>                  , s_gnuplotOutFile = oFile
>                  , s_ftype = fType
>                  , s_increments = incr
>                  , s_cutoffMax = mcutoff } = settings
>     suite   <- getParseFromFile geniTestSuite sFile
>     let rawPoints = map numOutputs suite
>         cutoff = fromMaybe (maximum rawPoints) mcutoff
>         -- clump the data points by increments
>         xTics  = takeWhile (<= cutoff) $ iterate (+ incr) incr
>         points = pointsFor rawPoints xTics
>     putStrLn $ toGnuPlot fType oFile points
>  where
>   pointsFor  ps ts     = reverse $ pointsForH ps (reverse ts)
>   pointsForH ps []     = [(0, length ps)]
>   pointsForH ps (t:ts) = point : pointsForH smaller ts
>     where point = (t, length bigger)
>           (bigger, smaller) = partition (> t) ps
>
>   numOutputs = length . tcOutputs

We generate an entire GnuPlot plt file (as opposed to a bunch dat)
because there is enough stuff to parameterise that we might as well
let the program do it.

> toGnuPlot :: String      -- ^ for example, postscript or png
>           -> FilePath    -- ^ output file name
>           -> [(Int,Int)] -- ^ data points
>           -> String
> toGnuPlot outType outFile ps =
>  unlines $
>    [ "set terminal " ++ outType
>    , "set output \""   ++ outFile ++ "\""
>    , "set xtics rotate"
>    , "set ytics rotate"
>    , "set nokey"
>    , "set boxwidth 0.75"
>    , "set style fill solid border -1"
>    , "set xlabel \"num paraphrases\""
>    , "set ylabel \"num cases\""
>   ] ++ toGnuPlot' ps
>
> toGnuPlot' :: [(Int,Int)] -> [String]
> toGnuPlot' [] = []
> toGnuPlot' ps =
>    [ "set xtics (" ++ (concat $ intersperse ", " xTics) ++ ")"
>    , "set yrange [ " ++ show y0 ++ ":" ++ show y1 ++ " ]"
>    , "plot \"-\" using 1:2 with boxes fill"
>    ] ++ zipWith (\pos xy -> show pos ++ " " ++ (show.snd) xy) positions ps
>  where
>    positions = [ 1::Integer .. ]
>    xTics = zipWith showTic (map fst ps) positions
>    showTic x p = "\"> " ++ show x ++ "\" " ++ show p
>    minmax l = (minimum l - 1, maximum l + 1)
>    (y0, y1) = minmax . map snd $ ps


Command line arguments
----------------------

> data Settings = Settings
>        { s_suiteFile  :: FilePath
>        , s_ftype      :: String
>        , s_gnuplotOutFile :: FilePath
>        , s_increments :: Int
>        , s_cutoffMax  :: Maybe Int
>        }
>
> emptySettings :: Settings
> emptySettings = Settings { s_suiteFile = ""
>                          , s_ftype = ""
>                          , s_gnuplotOutFile = ""
>                          , s_increments = 1
>                          , s_cutoffMax = Nothing
>                          }
>
> options :: [OptDescr (Settings -> Settings)]
> options =
>  [ Option []  ["suite"]       (ReqArg (\x s -> s { s_suiteFile = x })   "FILE") "test suite FILE (input)"
>  , Option []  ["gnuplot-out"] (ReqArg (\x s -> s { s_gnuplotOutFile = x })  "FILE") "gnuplot output FILE (not the output of this program)"
>  , Option []  ["type"]        (ReqArg (\x s -> s { s_ftype = x }) "STRING")     "file type STRING (e.g. 'postscript enhanced')"
>  , Option []  ["increments"]  (ReqArg (\x s -> s { s_increments = read x}) "INT")    "increments of INT"
>  , Option []  ["maximum"]     (ReqArg (\x s -> s { s_cutoffMax = Just $ read x}) "INT")     "cut off at max INT"
>  ]
>
> readArgv :: IO Settings
> readArgv =
>   do pname <- getProgName
>      argv  <- getArgs
>      case getOpt Permute options argv of
>       (os,_,[]  )
>         | notSet s_ftype          -> help pname ["file type must be set"]
>         | notSet s_gnuplotOutFile -> help pname ["gnuplot output must be set"]
>         | notSet s_suiteFile      -> help pname ["test suite be set"]
>         | otherwise               -> return settings
>         where
>          notSet x = null (x settings)
>          settings = foldr ($) emptySettings os
>       (_,_,errs)  -> help pname errs
>  where
>    help pname errs =
>     ioError (userError (concat errs ++ usageInfo header options))
>     where header = "Usage: " ++ pname ++ " [OPTION...]"


Basic bureaucracy

> getParseFromFile :: Parser b -> FilePath -> IO b
> getParseFromFile p f =
>   parseFromFile p f >>= either exitShowing return
>
> exitShowing :: (Show a) => a -> IO b
> exitShowing err=
>  do let err_ = show err
>     ePutStrLn err_
>     exitFailure
