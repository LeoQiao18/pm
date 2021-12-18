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
-- getNextMove :: Board -> Player -> Depth -> Board
-- getNextMove board player depth = case getOptimalMoves tree of
--   (x : _) -> getTreeBoard x
--   []      -> error "no minimax nodes match with the value queried"
--  where
--   tree = calculateTree board player depth
--   getOptimalMoves t = case t of
--     MinimaxNode maxVal _ children ->
--       filter (\c -> (getTreeVal c) == maxVal) children
--     MinimaxLeaf _ _ -> error "no viable next moves left"

-- -- construct minimax tree given board, and evaluate each node based on minimax strategy
-- calculateTree :: Board -> Player -> Depth -> MinimaxTree
-- calculateTree board player depth = constructTree 1 player board
--  where
--   constructTree curDepth curPlayer curBoard
--     | curDepth == depth
--     = let evalRes = evaluateBoard curBoard player
--       in  MinimaxLeaf evalRes curBoard
--     | otherwise
--     = let nextMoves = getNextMoves curBoard curPlayer
--       in
--         let
--           children =
--             map (constructTree (curDepth + 1) (otherPlayer curPlayer)) nextMoves
--         in  let evalRes = evaluateFromChildren children curDepth
--             in  MinimaxNode evalRes curBoard children
--   evaluateFromChildren children d | d `mod` 2 == 1 = maximum vals
--                                   | otherwise      = minimum vals
--     where vals = map getTreeVal children

-- -- evaluate board according to the quantity and quality of pieces relative to player
-- evaluateBoard :: Board -> Player -> Int
-- evaluateBoard board player = foldl addValue 0 allPos
--  where
--   allPos = [ (x, y) | x <- [1 .. 8], y <- [1 .. 8] ]
--   addValue sum pos = case board `atPos` pos of
--     Nothing -> sum
--     Just (curPlayer, piece) ->
--       let mult = if player == curPlayer then 1 else (-1)
--       in  sum + mult * pieceValue piece
--   pieceValue piece = case piece of
--     Pawn   -> 10
--     Knight -> 30
--     Bishop -> 30
--     Rook   -> 50
--     Queen  -> 90
--     King   -> 200

-- -- get all possible next game states for the current player
-- getNextMoves :: Board -> Player -> [Board]
-- getNextMoves board@(Board b) player = do
--   pos <- filter isValid allPos
--   getNextMovesForPosition pos
--  where
--   isValid pos = case board `atPos` pos of
--     Nothing             -> False
--     Just (curPlayer, _) -> curPlayer == player
--   allPos = [ (x, y) | x <- [1 .. 8], y <- [1 .. 8] ]
--   getNextMovesForPosition pos = map (nextPosToMove pos) allNextPos
--    where
--     elem = board `atPos` pos
--     nextPosToMove pos nextPos =
--       let b' = setElem elem nextPos b
--       in  let b'' = setElem Nothing pos b' in Board b''
--     allNextPos = getAllNextPos board pos elem

-- -- get all valid next positions for a certain piece on the board
-- getAllNextPos :: Board -> (Int, Int) -> BoardPiece -> [(Int, Int)]
-- getAllNextPos board pos@(x, y) Nothing                = []
-- getAllNextPos board pos@(x, y) (Just (player, piece)) = case piece of
--   -- TODO: pawn can jump 2 blocks on first move
--   Pawn   -> filter inRange $ movePawnToFront ++ movePawnToDiagonal
--   Knight -> filter
--     inRange
--     [ (x + 1, y + 2)
--     , (x + 1, y - 2)
--     , (x + 2, y + 1)
--     , (x + 2, y - 1)
--     , (x - 1, y + 2)
--     , (x - 1, y - 2)
--     , (x - 2, y + 1)
--     , (x - 2, y - 1)
--     ]
--   Bishop -> do
--     dir <- [(1, 1), (1, -1), (-1, 1), (-1, -1)]
--     allPosInDirection 1 dir
--   Rook -> do
--     dir <- [(1, 0), (-1, 0), (0, 1), (0, -1)]
--     allPosInDirection 1 dir
--   Queen -> do
--     dir <-
--       [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]
--     allPosInDirection 1 dir
--   King -> filter
--     inRange
--     [ (x', y')
--     | x' <- [(x - 1) .. (x + 1)]
--     , y' <- [(y - 1) .. (y + 1)]
--     , (x', y') /= (x, y)
--     ]
--  where
--   inRange (x', y') = x' >= 1 && x' <= 8 && y' >= 1 && y' <= 8
--   movePawnToFront | Data.Maybe.isJust $ board `atPos` destPos = []
--                   | otherwise = [destPos]
--     where destPos = (x + pawnDirByPlayer player, y)
--   movePawnToDiagonal = [ pos | pos <- diags, canCapture $ board `atPos` pos ]
--    where
--     diags = filter
--       inRange
--       [(x + pawnDirByPlayer player, y + 1), (x + pawnDirByPlayer player, y - 1)]
--     canCapture Nothing                 = False
--     canCapture (Just (otherPlayer, _)) = otherPlayer /= player
--   pawnDirByPlayer p = if p == Black then 1 else (-1)
--   allPosInDirection mult (xDir, yDir)
--     | x' < 1 || x' > 8 || y' < 1 || y' > 8 = []
--     | Data.Maybe.isJust dest = case dest of
--       Just (otherPlayer, _) -> [ (x', y') | otherPlayer /= player ]
--       _                     -> [(x', y')]
--     | otherwise = (x', y') : allPosInDirection (mult + 1) (xDir, yDir)
--    where
--     (x', y') = (x + mult * xDir, y + mult * yDir)
--     dest     = board `atPos` (x', y')
