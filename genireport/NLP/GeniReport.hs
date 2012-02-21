-- GenI reporting tool
-- Copyright (C) 2011 Eric Kow (Computational Linguistics Ltd)
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

import Control.Applicative
import Control.Monad (forM_)
import Data.Char ( isDigit, toLower )
import Data.Function ( on )
import Data.List ( intersperse, intercalate, sort, group, nub, sortBy )
import Data.List.Split

import Text.Blaze.Html5 hiding ( map )
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 ( renderHtml )

import qualified Data.ByteString.Lazy as B
import System.Console.CmdLib hiding ( group )
import qualified System.Console.CmdLib as C
import System.Directory
import System.FilePath
import System.IO.Strict
import Prelude hiding ( readFile )
import qualified Prelude as P 
import Control.Monad
import Text.JSON hiding ( Result )
import qualified Text.JSON as J

import NLP.GenI
import NLP.GenI.GeniParsers
import NLP.GenI.GeniShow
import NLP.GenI.Semantics
import Paths_genireport

data GeniReport = GeniReport
  { inputDir  :: FilePath
  , outputDir :: FilePath
  }
  deriving (Typeable, Data, Eq)

instance Attributes GeniReport where
  attributes _ = C.group "Options" [
      inputDir %> [ Positional 0 
                  , Help "GenI batch directory", ArgHelp "DIR" ],
      outputDir %> [ Positional 1
                    , Help "Output directory", ArgHelp "DIR" ]
    ]

instance RecordCommand GeniReport where
  mode_summary _ = "GeniReport input-dir output.html"

main = do
  opts <- executeR GeniReport {} =<< getArgs
  res  <- readResults (inputDir opts)
  let odir = outputDir opts
  B.writeFile (odir </> "report.html")  $ renderHtml (mkSummary res)
  B.writeFile (odir </> "details.html") $ renderHtml (mkDetailsSummary res)
  forM dataFiles $ \bn -> do
    f <- getDataFileName bn
    copyFile f (odir </> bn)

readResults d = do
  cases <- getRealDirectoryContents d
  forM cases $ \c -> do
    let dc = d </> c
    mderivs <- readFileIfExists (J.Ok []) J.decode (dc </> "derivations")
    derivs  <- case mderivs of
                 J.Error err -> fail err
                 J.Ok x      -> return x
    msem <- parseFromFile geniSemanticInput (dc </> "semantics")
    sem  <- case msem of
              Left err -> fail (show err)
              Right s  -> return s
    Result c sem
             <$> (lines `fmap` readFile (dc </> "responses"))
             <*> readFileIfExists [] lines (dc </> "warnings")
             <*> pure derivs

-- ----------------------------------------------------------------------
-- business
-- ----------------------------------------------------------------------

data Result = Result
  { reKey          :: String
  , reSemInput     :: SemInput
  , reRealisations :: [String]
  , reWarnings     :: [String]
  , reDerivation   :: [GeniResult]
  }

-- deriving Show
-- instance Show Result where

mkSummary :: [Result] -> Html
mkSummary res = html $ do
  H.head $ do
    H.link ! rel "stylesheet" ! type_  "text/css" ! href "report.css"
    H.script "" ! type_ "text/javascript" ! src "jquery-1.6.2.min.js"
    H.script "" ! type_ "text/javascript" ! src "jquery.tablesorter.min.js"
    H.script . toHtml . unlines $
     [ "$(document).ready(function()"
     , " {"
     , "   $(\"#resultsTable\").tablesorter();"
     , " }"
     , ");"
     ]
  body $ resultsTable res

resultsTable :: [Result] -> Html
resultsTable rs =
  table content ! A.id "resultsTable" ! class_ "tablesorter"
 where
  content = do
   thead . tr $ do
           th "case" ! colspan "2"
           th "results"
           th "warnings"
   tbody $ forM_ rs resultsRow

resultsRow :: Result -> Html
resultsRow r@(Result {..}) = tr cells
 where
  cells = do
   td (return ()) ! class_ (status r)
   td (prettyKey reKey)
   td (toHtml (length reRealisations))
   td (toHtml (length reWarnings))

status :: Result -> AttributeValue
status (Result {..})
 | length reRealisations == 0 = "failure"
 | length reWarnings     >  0 = "warnings"
 | otherwise                  = "success"

mkDetailsSummary :: [Result] -> Html
mkDetailsSummary res = html $ do
  H.head $ do
    H.link ! rel "stylesheet" ! type_  "text/css" ! href "report.css"
    H.script "" ! type_ "text/javascript" ! src "jquery-1.6.2.min.js"
    H.script "" ! type_ "text/javascript" ! src "jquery.tablesorter.min.js"
    H.style . toHtml . unlines $
      [ "td { border-bottom-style: solid; border-bottom-width: 1px; }"
      , ".count { color: grey } "
      , ".mute  { color: grey } "
      ]
    H.script . toHtml . unlines $
     [ "$(document).ready(function()"
     , " {"
     , "   $(\"#detailsTable\").tablesorter();"
     , " }"
     , ");"
     ]
  detailsTable res

detailsTable :: [Result] -> Html
detailsTable rs =
  table content !  A.id "detailsTable" ! class_ "tablesorter"
 where
  content = do
   thead . tr $ do
           th "case"     ! colspan "2"
           th "results"  ! colspan "2"
           th "warnings" ! colspan "2"
   tbody $ forM_ rs detailsRow

detailsRow :: Result -> Html
detailsRow r@(Result {..}) = tr cells
 where
  cells = do
   td (return ()) ! class_ (status r) -- colour code
   td (H.span tcName ! A.style "width:60em; display: inline-block;") -- limit the width a bit
   td (toHtml (length reRealisations))
   td (toHtml (unlinesCountHtml reRealisations))
   td (toHtml (length reWarnings))
   td (toHtml (unlinesCountHtml . concatMap expandCount $ reWarnings))
  traces = [  lcSort . nub $ concatMap nlTrace $ grLexSelection g | GSuccess g <- reDerivation ]
  tcName = do
   prettyKey reKey
   br
   H.div (semInputToHtml reSemInput) ! A.style "margin-top: 1em;"
   br
   H.div (sequence_ . intersperse br $ map (toHtml . unwords) traces)

semInputToHtml :: SemInput -> Html
semInputToHtml (sem,icons,lcons) = do
  keyword "semantics"
  squares $ sequence_ . intersperse sp $ map withConstraints sem
  unless (null icons) $ do
    br
    keyword "idxconstraints"
    squares $ toHtml (geniShow icons)
 where
  keyword :: String -> Html
  keyword txt = H.span (toHtml txt)
  mute        = class_ "mute"
  sp = toHtml (" " :: String)
  --
  withConstraints lit = toHtml lit >> constraints lit
  constraints lit =
    case concat [ cs | (p,cs) <- lcons, p == lit ] of
      [] -> return ()
      cs -> squares (toHtml (unwords cs) ! mute)
  --
  squares x = do
    H.span (toHtml ("[" :: String))
    x
    H.span (toHtml ("]" :: String))

instance ToHtml Literal where
 toHtml (Literal h p l) = do
   H.span (toHtml (geniShow h ++ ":")) ! mute
   toHtml (geniShow p ++ "(" ++ unwords (map geniShow l) ++ ")")
  where
   mute        = class_ "mute"

prettyKey :: String -> Html
prettyKey = toHtml

expandCount :: String -> [String]
expandCount x = 
  case dropPrefix (reverse suff) (reverse x) of
   ("", ry) -> let (rc, rmsg) = P.span isDigit ry
                   msg   = reverse (drop 2 rmsg)
                   count = read (reverse rc)
               in replicate count msg
   (_,  _)  -> [x]
 where
  suff = " times)"

unlinesCountHtml :: [String] -> Html
unlinesCountHtml = sequence_ . intersperse br . map htmlC . groupAndCount
 where
  htmlC (s, 1) = toHtml s
  htmlC (s, c) = toHtml s >> " " >> H.span ("⨉" >> toHtml c) ! class_ "count"

-- ----------------------------------------------------------------------

dataFiles :: [FilePath]
dataFiles =
  [ "jquery-1.6.2.min.js"
  , "jquery.tablesorter.min.js"
  , "asc.gif"
  , "bg.gif"
  , "desc.gif"
  , "report.css"
  ]

-- ----------------------------------------------------------------------
-- odds and ends
-- ----------------------------------------------------------------------

lcSort :: [String] -> [String]
lcSort = sortBy (compare `on` map toLower)

readFileIfExists :: a -> (String -> a) -> FilePath -> IO a
readFileIfExists z job f = do
  x <- doesFileExist f
  if x then job <$> readFile f
       else return z

groupAndCount :: (Eq a, Ord a) => [a] -> [(a, Int)]
groupAndCount xs = 
  map (\x -> (P.head x, length x)) grouped
  where grouped = (group . sort) xs

dropPrefix :: Eq a => [a] -> [a] -> ([a],[a])
dropPrefix (x:xs) (y:ys) | x == y    = dropPrefix xs ys
dropPrefix left right = (left,right)

getRealDirectoryContents :: FilePath -> IO [String]
getRealDirectoryContents d =
  filter (not . (`elem` [".",".."])) <$> getDirectoryContents d
