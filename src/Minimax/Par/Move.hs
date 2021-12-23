-- | Move generation by parallel minimax algorithm

module Minimax.Par.Move
  ( bestMove
  ) where

import           Chess                                    ( Game(..)
                                                          , Player(..)
                                                          )
import           Minimax.Common                           ( Depth )
import           Rules                                    ( legalMoves )
import           Score                                    ( Score
                                                          , gameScore
                                                          )

import           Control.Parallel.Strategies              ( evalTuple2
                                                          , parList
                                                          , rseq
                                                          , using
                                                          )

bestMove :: Depth -> Depth -> Game -> Game
bestMove parDepth depth g =
  let evalStrat = if parDepth > 0 then parList (evalTuple2 rseq rseq) else rseq
      movesWithScores =
        map (\move -> (move, minimax (parDepth - 1) (depth - 1) move))
            (legalMoves g)
          `using` evalStrat
      comparator = if shouldMaximize g
        then \x@(_, xscore) y@(_, yscore) -> if xscore >= yscore then x else y
        else \x@(_, xscore) y@(_, yscore) -> if xscore <= yscore then x else y
      optimalMove = fst $ foldr1 comparator movesWithScores
  in  optimalMove

shouldMaximize :: Game -> Bool
shouldMaximize Game { gamePlayer = White } = True
shouldMaximize Game { gamePlayer = Black } = False

minimax :: Depth -> Depth -> Game -> Score
minimax parDepth depth g
  | depth > 0
  = let
      evalStrat = if parDepth > 0 then parList rseq else rseq
      scores =
        map (minimax (parDepth - 1) (depth - 1)) (legalMoves g)
          `using` evalStrat
      optimalScore =
        if shouldMaximize g then maximum scores else minimum scores
    in
      optimalScore
  | otherwise
  = gameScore g