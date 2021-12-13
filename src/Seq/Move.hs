-- | Move generation by sequential minimax algorithm

module Seq.Move
  ( getAllNextPos,
    getNextMoves,
    evaluateBoard
  ) where

import           Chess                                    ( Board(..)
                                                          , Game(..)
                                                          , BoardPiece(..)
                                                          , Piece(..)
                                                          , Player(..)
                                                          , Position(..)
                                                          , atPos, Game (gameBoard, gamePlayer)
                                                          )
import           Minimax                                  ( Depth
                                                          , MinimaxTree(..)
                                                          )
import           Data.Matrix                              ( setElem )
import qualified Data.Maybe

getNextMove :: Board -> Board
getNextMove = undefined

-- calculateTree :: Game -> Depth -> MinimaxTree
-- calculateTree game depth = let tree = constructTree 1 game
--   where constructTree curDepth curGame
--           | curDepth == depth = MinimaxLeaf 0 curGame
--           | otherwise = let nextMoves = getNextMoves curGame
--                         in MinimaxNode 0 curGame (map (constructTree (curDepth+1)) nextMoves)
          

evaluateBoard :: Board -> Player -> Int
evaluateBoard board player = foldl addValue 0 allPos
  where allPos = [(x,y) | x <- [1..8], y <- [1..8]]
        addValue sum pos = case board `atPos` pos of
          Nothing -> sum
          Just (curPlayer, piece) -> let mult = if player == curPlayer then 1 else (-1) in
                                     sum + mult * pieceValue piece
        pieceValue piece = case piece of
          Pawn -> 10
          Knight -> 30
          Bishop -> 30
          Rook -> 50
          Queen -> 90
          King -> 200

-- get all possible next game states for the current player
getNextMoves :: Game -> [Game]
getNextMoves game@(Game {gamePlayer=player, gameBoard=board@(Board b)}) = 
  do pos <- filter isValid allPos
     getNextMovesForPosition game pos
  where isValid pos = case board `atPos` pos of
          Nothing -> False 
          Just (curPlayer, _) -> curPlayer == player
        allPos = [ (x,y) | x <- [1..8], y <- [1..8] ]
        getNextMovesForPosition game pos = map (nextPosToMove pos) allNextPos
          where elem = board `atPos` pos
                nextPosToMove pos nextPos = let b' = setElem elem nextPos b in 
                                            let b'' = setElem Nothing pos b' in
                                            Game {gamePlayer = otherPlayer player, gameBoard = Board b''}
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

otherPlayer :: Player -> Player 
otherPlayer Black = White 
otherPlayer White = Black