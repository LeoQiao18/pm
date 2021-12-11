-- | Move generation by sequential minimax algorithm

module Seq.Move
  ( printSeq
  ) where

import           Chess                                    ( Board(..)
                                                          , Piece(..)
                                                          , Player(..)
                                                          , Position(..)
                                                          )
import           Minimax                                  ( Depth
                                                          , MinimaxTree(..)
                                                          )

printSeq :: IO ()
printSeq = putStrLn "Seq.Move"

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
