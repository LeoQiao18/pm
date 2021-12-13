-- | Move generation by sequential minimax algorithm

module Seq.Move
  ( getAllNextPos,
    getNextMoves
  ) where

import           Chess                                    ( Board(..)
                                                          , BoardPiece(..)
                                                          , Piece(..)
                                                          , Player(..)
                                                          , Position(..)
                                                          , atPos
                                                          )
import           Minimax                                  ( Depth
                                                          , MinimaxTree(..)
                                                          )
import           Data.Matrix                              ( setElem )
import qualified Data.Maybe
import           Control.Monad                            ( guard )

getNextMove :: Board -> Board
getNextMove = undefined

calculateTree :: Board -> Depth -> MinimaxTree
calculateTree = undefined

evaluateBoard :: Board -> Int
evaluateBoard = undefined

-- calculate moves
getNextMoves :: Board -> Player -> [Board]
getNextMoves board curPlayer = do pos <- filter isCand allPos
                                  getNextMovesForPosition board pos
  where isCand pos = case board `atPos` pos of
          Nothing -> False 
          Just (player, _) -> player == curPlayer
        allPos = [ (x,y) | x <- [1..8], y <- [1..8] ]

getNextMovesForPosition :: Board -> Position -> [Board]
getNextMovesForPosition board@(Board b) pos = map nextPosToMove allNextPos
  where nextPosToMove nextPos = let b' = setElem elem nextPos b in Board (setElem Nothing pos b')
        elem = board `atPos` pos
        allNextPos = getAllNextPos board pos elem

-- get all valid next positions for a certain piece on the board
getAllNextPos :: Board -> (Int, Int) -> BoardPiece -> [(Int, Int)]
getAllNextPos board pos@(x,y) Nothing = []
getAllNextPos board pos@(x,y) (Just (player, piece)) = case piece of
  -- TODO: pawn can jump 2 blocks on first move
  Pawn -> filter inRange $ movePawnToFront ++ movePawnToDiagonal
  Knight -> filter inRange [(x+1,y+2),(x+1,y-2),(x+2,y+1),(x+2,y-1),(x-1,y+2),(x-1,y-2),(x-2,y+1),(x-2,y-1)]
  Bishop -> do dir <- [(1,1),(1,-1),(-1,1),(-1,-1)]
               allPosInDirection 1 dir
  Rook -> do dir <- [(1,0),(-1,0),(0,1),(0,-1)]
             allPosInDirection 1 dir
  Queen -> do dir <- [(1,0),(-1,0),(0,1),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)]
              allPosInDirection 1 dir
  King -> filter inRange [(x',y')|x' <- [(x-1)..(x+1)], y' <- [(y-1)..(y+1)], (x',y') /= (x,y)] 
  where
    inRange (x',y') = x' >= 1 && x' <= 8 && y' >= 1 && y' <= 8 
    movePawnToFront
      | Data.Maybe.isJust $ board `atPos` destPos = []
      | otherwise = [destPos]
      where destPos = (x+pawnDirByPlayer player, y)
    movePawnToDiagonal = [pos | pos <- diags, canCapture $ board `atPos` pos]
      where diags = filter inRange [(x+pawnDirByPlayer player,y+1), (x+pawnDirByPlayer player,y-1)]
            canCapture Nothing = False 
            canCapture (Just (otherPlayer, _)) = otherPlayer /= player
    pawnDirByPlayer p = if p == Black then 1 else (-1)
    allPosInDirection mult (xDir,yDir)
      | x' < 1 || x' > 8 || y' < 1 || y' > 8 = []
      | Data.Maybe.isJust dest = case dest of
          Just (otherPlayer,_) -> [(x', y') | otherPlayer /= player]
          _ -> [(x',y')]
      | otherwise = (x',y'):allPosInDirection (mult+1) (xDir,yDir)
      where (x',y') = (x+mult*xDir, y+mult*yDir)
            dest = board `atPos` (x',y')