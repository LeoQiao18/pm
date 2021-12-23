-- | Move generation by parallel minimax algorithm with alpha-beta pruning

module Minimax.ParAB.Move
  ( bestMove
  ) where

import           Chess                                    ( Game(..)
                                                          , Player(..)
                                                          )
import           Control.Parallel.Strategies              ( evalTuple2
                                                          , parList
                                                          , rseq
                                                          , using
                                                          )
import           Minimax.Common                           ( Depth )
import           Rules                                    ( legalMoves )
import           Score                                    ( Score
                                                          , gameScore
                                                          )

bestMove :: Depth -> Depth -> Game -> Game
bestMove parDepth d g
  | parDepth <= 1
  = let
      movesWithScores =
        [ (move, minimaxAB (d - 1) (-10000) 10000 move) | move <- legalMoves g ]
        `using` parList (evalTuple2 rseq rseq)
      comparator = if shouldMaximize g
        then \x@(_, xscore) y@(_, yscore) -> if xscore >= yscore then x else y
        else \x@(_, xscore) y@(_, yscore) -> if xscore <= yscore then x else y
      optimalMove = fst $ foldr1 comparator movesWithScores
    in
      optimalMove
  | otherwise
  = let movesWithScores =
          [ (move, minimax (parDepth - 1) (d - 1) move) | move <- legalMoves g ]
          `using` parList (evalTuple2 rseq rseq)
        comparator = if shouldMaximize g
          then \x@(_, xscore) y@(_, yscore) -> if xscore >= yscore then x else y
          else \x@(_, xscore) y@(_, yscore) -> if xscore <= yscore then x else y
        optimalMove = fst $ foldr1 comparator movesWithScores
    in  optimalMove

shouldMaximize :: Game -> Bool
shouldMaximize Game { gamePlayer = White } = True
shouldMaximize Game { gamePlayer = Black } = False

minimax :: Depth -> Depth -> Game -> Score
minimax parDepth d g
  | d <= 0
  = gameScore g
  | parDepth <= 1
  = let scores =
          [ minimaxAB (d - 1) (-10000) 10000 move | move <- legalMoves g ]
          `using` parList rseq
        optimalScore =
          if shouldMaximize g then maximum scores else minimum scores
    in  optimalScore
  | otherwise
  = let scores =
          [ minimax (parDepth - 1) (d - 1) move | move <- legalMoves g ]
          `using` parList rseq
        optimalScore =
          if shouldMaximize g then maximum scores else minimum scores
    in  optimalScore

minimaxAB :: Depth -> Score -> Score -> Game -> Score
minimaxAB d alpha beta g
  | d <= 0
  = gameScore g
  | shouldMaximize g
  = let optimalScore _ prevBest [] = prevBest
        optimalScore alpha' prevBest (move : moves) =
          let currBest = max prevBest (minimaxAB (d - 1) alpha' beta move)
              alpha''  = max alpha' currBest
          in  if beta <= alpha''
                then currBest
                else optimalScore alpha'' currBest moves
    in  optimalScore alpha (-9999) (legalMoves g)
  | otherwise
  = let optimalScore _ prevBest [] = prevBest
        optimalScore beta' prevBest (move : moves) =
          let currBest = min prevBest (minimaxAB (d - 1) alpha beta' move)
              beta''   = min beta' currBest
          in  if beta'' <= alpha
                then currBest
                else optimalScore beta'' currBest moves
    in  optimalScore beta 9999 (legalMoves g)