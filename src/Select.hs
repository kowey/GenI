-- GenI surface realiser
-- Copyright (C) 2006 Carlos Areces and Eric Kow
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

-- | This module is meant to act as a standalone program which does
-- lexical selection and nothing more.
--
-- The output consists of a newline-delimited list of families
--
-- Note that we make no guarantees about how many times an entry
-- or a family may appear.  You may want to do your own sorting
-- It isn't used in the GenI pipeline per se, but can be useful for
-- other applications

module Main (main) where

import System(getArgs)
import Data.List(intersperse)
import Data.IORef(newIORef, readIORef, modifyIORef)

import NLP.GenI.Btypes(SemInput, ILexEntry(ifamname), TestCase(tcName,tcSem))
import NLP.GenI.General(fst3)
import NLP.GenI.Geni(emptyProgState, ProgState(le, pa, ts, tcase, tsuite), ProgStateRef,
                     loadEverything, chooseLexCand,)
import NLP.GenI.Configuration(treatArgs, getListFlagP, OutputFileFlg(..),
    Params(grammarType), GrammarType(PreCompiled))


main :: IO ()
main =
 do confArgs <- treatArgs =<< getArgs
    pstRef   <- newIORef . emptyProgState $ confArgs { grammarType = PreCompiled }
    loadEverything pstRef
    --
    pst <- readIORef pstRef
    let pstCase  = tcase pst
        suite    = tsuite pst
    inputs <- if null pstCase
                 then return $ map tcSem suite
                 else case [ tcSem tc | tc <- suite, tcName tc == pstCase ] of
                      [] -> fail ("No such test case: " ++ pstCase)
                      cs -> return cs
    sequence_ $ map (selectOnSemInput pstRef) inputs

-- | Write the lexical selection for a sem input onto either
--   the stdout or the output location
selectOnSemInput :: ProgStateRef -> SemInput -> IO ()
selectOnSemInput pstRef semInput =
  do modifyIORef pstRef (\x -> x{ts = semInput})
     pst <- readIORef pstRef
     let selection = chooseLexCand (le pst) (fst3 semInput)
     --
     let config = pa pst
         out    = getListFlagP OutputFileFlg config
         oPutStrLn = if null out then putStrLn else writeFile out
     oPutStrLn . unlines . (map showLex) $ selection

showLex :: ILexEntry -> String
showLex i = concat $ intersperse " | " [ifamname i]
-- $ (ifamname i):(iword i)
