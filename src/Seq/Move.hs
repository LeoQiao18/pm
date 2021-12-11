-- | Move generation by sequential minimax algorithm

module Seq.Move
  () where

import           Chess                                    ( Board(..)
                                                          , Piece(..)
                                                          , Player(..)
                                                          , Position(..)
                                                          )
import           Minimax                                  ( Depth
                                                          , MinimaxTree(..)
                                                          )

getNextMove :: Board -> Board
getNextMove = undefined

calculateTree :: Board -> Depth -> MinimaxTree
calculateTree = undefined

evaluateBoard :: Board -> Int
evaluateBoard = undefined

-- calculate moves
getNextMoves :: Board -> [Board]
getNextMoves = undefined

getNextMovesForPosition :: Board -> Position -> [Board]
getNextMovesForPosition = undefined
