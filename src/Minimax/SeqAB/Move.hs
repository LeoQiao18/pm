-- | Move generation by sequential minimax algorithm with alpha-beta pruning

module Minimax.SeqAB.Move
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
  let movesWithScores =
        [ (move, minimax (d - 1) (-10000) 10000 move) | move <- legalMoves g ]
      comparator = if shouldMaximize g
        then \x@(_, xscore) y@(_, yscore) -> if xscore >= yscore then x else y
        else \x@(_, xscore) y@(_, yscore) -> if xscore <= yscore then x else y
      optimalMove = fst $ foldr1 comparator movesWithScores
  in  optimalMove

shouldMaximize :: Game -> Bool
shouldMaximize Game { gamePlayer = White } = True
shouldMaximize Game { gamePlayer = Black } = False

minimax :: Depth -> Score -> Score -> Game -> Score
minimax d alpha beta g
  | d <= 0
  = gameScore g
  | shouldMaximize g
  = let optimalScore _ prevBest [] = prevBest
        optimalScore alpha' prevBest (move : moves) =
          let currBest = max prevBest (minimax (d - 1) alpha' beta move)
              alpha''  = max alpha' currBest
          in  if beta <= alpha''
                then currBest
                else optimalScore alpha'' currBest moves
    in  optimalScore alpha (-9999) (legalMoves g)
  | otherwise
  = let optimalScore _ prevBest [] = prevBest
        optimalScore beta' prevBest (move : moves) =
          let currBest = min prevBest (minimax (d - 1) alpha beta' move)
              beta''   = min beta' currBest
          in  if beta'' <= alpha
                then currBest
                else optimalScore beta'' currBest moves
    in  optimalScore beta 9999 (legalMoves g)