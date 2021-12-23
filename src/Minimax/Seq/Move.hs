-- | Move generation by sequential minimax algorithm

module Minimax.Seq.Move
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

bestMove :: Depth -> Game -> Game
bestMove d g =
  let movesWithScores = [ (move, minimax (d - 1) move) | move <- legalMoves g ]
      comparator      = if shouldMaximize g
        then \x@(_, xscore) y@(_, yscore) -> if xscore >= yscore then x else y
        else \x@(_, xscore) y@(_, yscore) -> if xscore <= yscore then x else y
      optimalMove = fst $ foldr1 comparator movesWithScores
  in  optimalMove

shouldMaximize :: Game -> Bool
shouldMaximize Game { gamePlayer = White } = True
shouldMaximize Game { gamePlayer = Black } = False

minimax :: Depth -> Game -> Score
minimax d g
  | d <= 0
  = gameScore g
  | otherwise
  = let scores = [ minimax (d - 1) move | move <- legalMoves g ]
        optimalScore =
          if shouldMaximize g then maximum scores else minimum scores
    in  optimalScore