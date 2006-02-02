----------------------------------------------------
--                                                --
-- Statistics.hs:                                 --
-- Functions that collect and print out           --
-- statistics                                     --
--                                                --
----------------------------------------------------

{-
Copyright (C) HyLoRes 2002-2005
Carlos Areces     - areces@loria.fr      - http://www.loria.fr/~areces
Daniel Gorin      - dgorin@dc.uba.ar
Juan Heguiabehere - juanh@inf.unibz.it - http://www.inf.unibz.it/~juanh/

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
USA.
-}

module Statistics(Statistics, StatisticsState, StatisticsStateIO,

    recordGivenClause, recordNewClauseRep,
    recordFiredRule, recordSubsumedPremise,

    printOutAllMetrics, printOutAllMetrics', printOutInspectionMetrics,

    initialStatisticsStateFor,
    addMetric, addInspectionMetric, setPrintOutInterval,

    Metric,
    rawClausesGenerated, nonFwSubsumedClausesGenerated,
    premisesSubsumedByConsequents, ruleApplicationCount,
    averageGivenClauseSize

) where

import Control.Monad.State

import Data.FiniteMap (FiniteMap, addToFM_C, fmToList, emptyFM)

import Base (separate)
import SimpleClause (SimpleClause)
import qualified SelClause as SelCl (size)
import ClauseRepository (TaggedClause, asSelClause)

import RuleMetadata(RuleId(..))


-------------------------------------------
-- Statistics are collections of Metrics
-- which can be printed out (at regular intervals)
-------------------------------------------
data Statistics = Stat{metrics::[Metric],
                       inspectionMetrics::[Metric],
                       count::Int,
                       step::Maybe Int}

type StatisticsState a   = forall m. (MonadState Statistics m) => m a
type StatisticsStateIO a = forall m. (MonadState Statistics m, MonadIO m) => m a

updateMetrics :: (Metric -> Metric) -> Statistics -> Statistics
updateMetrics f stat = stat{metrics           = map f (metrics stat),
                            inspectionMetrics = map f (inspectionMetrics stat)}

updateStep :: Statistics -> Statistics
updateStep s@(Stat _ [] _     _)         = s
updateStep s@(Stat _ _  _     Nothing)   = s
updateStep stat                          = stat{count = (count stat)+1}

needsToPrintOut :: Statistics -> Bool
needsToPrintOut (Stat _ [] _     _)         = False
needsToPrintOut (Stat _ _  _     Nothing)   = False
needsToPrintOut (Stat _ _  iter (Just toi)) = iter > 0 && iter `mod` toi == 0

noStats :: Statistics -> Bool
noStats (Stat [] [] _ _) = True
noStats  _               = False

emptyStats :: Statistics
emptyStats = Stat{metrics=[],
                  inspectionMetrics=[],
                  count=0,
                  step=Nothing}

--------------------------- Monadic Statistics functions follow ------------------------------


initialStatisticsStateFor :: (MonadState Statistics m) => (m a -> Statistics -> b) -> m a -> b
initialStatisticsStateFor f = flip f emptyStats

{- addMetric: - Adds a metric at the end of the list (thus,
   metrics are printed out in the order in which they were added -}
addMetric :: Metric -> StatisticsState ()
addMetric newMetric  = modify (\stat -> stat{metrics = (metrics stat)++[newMetric]})

{- addInspectionMetric: - Adds a metric that will be printed out
   at regular intervals -}
addInspectionMetric :: Metric -> StatisticsState ()
addInspectionMetric newMetric = modify (\stat -> stat{inspectionMetrics = (inspectionMetrics stat)++[newMetric]})

setPrintOutInterval :: Int -> StatisticsState ()
setPrintOutInterval i = modify (resetInterval i)
    where resetInterval 0 stat = stat{step = Nothing}
          resetInterval i stat = stat{step = Just i}

recordGivenClause :: TaggedClause -> StatisticsState ()
recordGivenClause cl  = do  modify (updateMetrics (recordGivenClauseM cl));
                            modify updateStep;

recordFiredRule :: RuleId -> [SimpleClause] -> StatisticsState ()
recordFiredRule rule cls = modify (updateMetrics $ recordFiredRuleM rule cls)

recordSubsumedPremise :: StatisticsState ()
recordSubsumedPremise = modify $ updateMetrics recordSubsumedPremiseM

recordNewClauseRep :: Int -> StatisticsState ()
recordNewClauseRep nextId  = modify $ updateMetrics (recordNewClauseRepM nextId)

printOutAllMetrics :: StatisticsStateIO ()
printOutAllMetrics = get >>= (liftIO . printOutAllMetrics')

printOutAllMetrics' :: Statistics -> IO ()
printOutAllMetrics' stats =
    do
        unless (noStats stats) $ do
            liftIO $ putStrLn "(final statistics)"
            liftIO $ printOutList (inspectionMetrics stats ++ metrics stats)

printOutInspectionMetrics :: StatisticsStateIO ()
printOutInspectionMetrics = do
                                shouldPrint <- gets needsToPrintOut
                                when ( shouldPrint ) $ do
                                    liftIO $ putStr "(partial statistics: iteration "
                                    iter <- gets count
                                    liftIO . putStr . show $ iter
                                    liftIO $ putStrLn ")"
                                    ims <- gets inspectionMetrics
                                    liftIO $ printOutList ims


printOutList :: Show a => [a] -> IO ()
printOutList ms = unless ( null ms ) $ do
                          let separator = "\n----------------------------------\n"
                          putStr "begin"
                          putStr separator
                          putStr (separate separator ms)
                          putStr separator
                          putStr "end\n"

--------------------------------------------
-- Metrics
--------------------------------------------
data Metric = CG  Int                    -- Raw clauses generated (sum of the clauses generated by each rule)
             |CG' Int                    -- Non fw-subsumed clauses generated
             |SP  Int                    -- Number of premises that were subsumed by their consequents
             |RC  (FiniteMap RuleId Int) -- Rule application count
             |GS  Int Int                -- Avg size of the given clause


instance Show Metric where
  show (CG  x)   = "Clauses generated (raw): " ++ (show x)
  show (CG' x)   = "Clauses generated (non forward-subsumed): " ++ (show x)
  show (SP  x)   = "Premises subsumed by their consequents: " ++ (show x)
  show (GS  s c) = "Avg. given clause size: " ++ (show (ratio s  c))
  show (RC  x)   = "Rule applications:" ++ concatMap p (fmToList x)
      where p (id,count) = "\n  " ++ (show id) ++ " rule: " ++ (show count)

recordGivenClauseM :: TaggedClause -> Metric -> Metric
recordGivenClauseM cl (GS s c) = GS (((s +) . SelCl.size . asSelClause) cl) (c+1)
recordGivenClauseM _ m = m

recordNewClauseRepM :: Int -> Metric -> Metric
recordNewClauseRepM nextId (CG' _) = CG' (nextId - 1)
recordNewClauseRepM _   m      = m

recordFiredRuleM :: RuleId -> [SimpleClause] -> Metric -> Metric
recordFiredRuleM _    cls (CG x)  = CG (x + length(cls))
recordFiredRuleM rule _   (RC fm) = RC (addToFM_C (+) fm rule 1)
recordFiredRuleM _    _    m      = m

recordSubsumedPremiseM :: Metric -> Metric
recordSubsumedPremiseM (SP x) = SP (x+1)
recordSubsumedPremiseM  m     = m

ratio :: Int -> Int -> Float
ratio x y = (fromIntegral x) / (fromIntegral y)

rawClausesGenerated, nonFwSubsumedClausesGenerated :: Metric
rawClausesGenerated           = CG  0
nonFwSubsumedClausesGenerated = CG' 0
premisesSubsumedByConsequents = SP  0
ruleApplicationCount          = RC  emptyFM
averageGivenClauseSize        = GS  0 0
