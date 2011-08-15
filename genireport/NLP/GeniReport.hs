-- GenI reporting tool
-- Copyright (C) 2011 Eric Kow (SRI)
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
import Data.Char ( isDigit )
import Data.List ( intersperse, intercalate, sort, group )
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
  B.writeFile (outputDir opts </> "report.html")  $ renderHtml (mkSummary res)
  B.writeFile (outputDir opts </> "details.html") $ renderHtml (mkDetailsSummary res)

readResults d = do
  cases <- getRealDirectoryContents d
  forM cases $ \c -> do
    let dc = d </> c
    Result c <$> (lines `fmap` readFile (dc </> "responses"))
             <*> (lines `fmap` readFile (dc </> "warnings"))
 
-- ----------------------------------------------------------------------
-- business
-- ----------------------------------------------------------------------

data Result = Result
  { reKey          :: String
  , reRealisations :: [String]
  , reWarnings     :: [String]
  } deriving Show

mkSummary :: [Result] -> Html
mkSummary res = html $ do
  H.head $
    H.style . toHtml . unlines $
      [ "td { border-bottom-style: solid; border-bottom-width: 1px; border-color: #ffffff}"
      , ".count { color: grey } "
      , ".warnings { background-color: #ffc343; }"
      , ".failure  { background-color: #f99; }"
      , ".success  { background-color: #8f8; }"
      ]
  body $ do
   resultsTable res

resultsTable :: [Result] -> Html
resultsTable rs = table $ do
  tr $ do th "case"
          th "results"
          th "warnings"
  forM_ rs resultsRow 

resultsRow :: Result -> Html
resultsRow (Result {..}) = tr cells ! class_ status 
 where
  cells = do
   td (prettyKey reKey)
   td (toHtml (length reRealisations))
   td (toHtml (length reWarnings))
  lenRealisations = length reRealisations
  lenWarnings     = length reWarnings
  status | lenRealisations == 0 = "failure"
         | lenWarnings     >  0 = "warnings"
         | otherwise            = "success"

mkDetailsSummary :: [Result] -> Html
mkDetailsSummary res = html $ do
  H.head $
    H.style . toHtml . unlines $
      [ "td { border-bottom-style: solid; border-bottom-width: 1px; }"
      , ".count { color: grey } "
      ]
  body $ do
   detailsTable res

detailsTable :: [Result] -> Html
detailsTable rs = table $ forM_ rs detailsRow

detailsRow :: Result -> Html
detailsRow (Result {..}) = tr $ do
 td (prettyKey reKey)
 td (toHtml (length reRealisations))
 td (toHtml (unlinesCountHtml reRealisations))
 td (toHtml (unlinesCountHtml . concatMap expandCount $ reWarnings))

prettyKey :: String -> Html
prettyKey k =
  mapM_ transform k
 where
  transform :: Char -> Html
  transform '+'               = " + " >> br
  transform x | x `elem` "-_" = " "
  transform x = toHtml x

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
-- odds and ends
-- ----------------------------------------------------------------------

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
