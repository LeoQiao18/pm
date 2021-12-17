-- | Chess rules

module Rules
    ( isGameOver
    , winner
    , legalMoves
    ) where

import           Chess                                    ( Board(..)
                                                          , Game(..)
                                                          , Piece(..)
                                                          , Player(..)
                                                          , Position
                                                          )
import           Data.Foldable                            ( find )
import           Data.Matrix                              ( (!)
                                                          , setElem
                                                          )
import           Data.Maybe                               ( fromJust )

isGameOver :: Game -> Bool
isGameOver Game { gameBoard = Board b } = not $ hasBlackKing && hasWhiteKing
  where
    hasBlackKing = Just (Black, King) `elem` b
    hasWhiteKing = Just (White, King) `elem` b

winner :: Game -> Maybe Player
winner g@Game { gameBoard = Board b } = if isGameOver g
    then Just . fst . fromJust . fromJust $ find isKing b
    else Nothing
  where
    isKing (Just (_, King)) = True
    isKing _                = False

legalMoves :: Game -> [Game]
legalMoves g =
    let allPositions = [ (r, c) | r <- [1 .. 8], c <- [1 .. 8] ]
    in  concatMap (legalMovesForPos g) allPositions


legalMovesForPos :: Game -> Position -> [Game]
legalMovesForPos g@Game { gamePlayer = player, gameBoard = Board b } pos@(x, y)
    = case b ! pos of
        Nothing -> []
        Just (piecePlayer, piece) ->
            if piecePlayer == player then movesForPiece piece else []
  where
    movesForPiece Pawn = case player of
        Black ->
            let normals = validEmpty [(x, y - 1)]
                takes   = validTake [(x - 1, y - 1), (x + 1, y - 1)]
                poses   = normals ++ takes
                newBoards (8, c) =
                    let promotions = [Pawn, Knight, Bishop, Rook, Queen]
                    in  map (makeMove pos (8, c)) promotions
                newBoards newpos = [makeMove pos newpos Pawn]
            in  map makeGame $ concatMap newBoards poses
        White ->
            let normals = validEmpty [(x, y + 1)]
                takes   = validTake [(x - 1, y + 1), (x + 1, y + 1)]
                poses   = normals ++ takes
                newBoards (0, c) =
                    let promotions = [Pawn, Knight, Bishop, Rook, Queen]
                    in  map (makeMove pos (0, c)) promotions
                newBoards newpos = [makeMove pos newpos Pawn]
            in  map makeGame $ concatMap newBoards poses
    movesForPiece Knight =
        let poses = validEmptyOrTake
                [ (x + 1, y + 2)
                , (x + 1, y - 2)
                , (x + 2, y + 1)
                , (x + 2, y - 1)
                , (x - 1, y + 2)
                , (x - 1, y - 2)
                , (x - 2, y + 1)
                , (x - 2, y - 1)
                ]
        in  map (\newpos -> makeGame $ makeMove pos newpos Knight) poses
    movesForPiece Bishop =
        let dir   = [(1, 1), (1, -1), (-1, 1), (-1, -1)]
            poses = concatMap (allPosInDirection 1) dir
        in  map (\newpos -> makeGame $ makeMove pos newpos Bishop) poses
    movesForPiece Rook =
        let dir   = [(1, 0), (-1, 0), (0, 1), (0, -1)]
            poses = concatMap (allPosInDirection 1) dir
        in  map (\newpos -> makeGame $ makeMove pos newpos Rook) poses
    movesForPiece Queen =
        let
            dir =
                [ (1 , 0)
                , (-1, 0)
                , (0 , 1)
                , (0 , -1)
                , (1 , 1)
                , (1 , -1)
                , (-1, 1)
                , (-1, -1)
                ]
            poses = concatMap (allPosInDirection 1) dir
        in
            map (\newpos -> makeGame $ makeMove pos newpos Queen) poses
    movesForPiece King =
        let poses = validEmptyOrTake
                [ (x', y')
                | x' <- [(x - 1) .. (x + 1)]
                , y' <- [(y - 1) .. (y + 1)]
                , (x', y') /= (x, y)
                ]
        in  map (\newpos -> makeGame $ makeMove pos newpos King) poses
    validEmpty = filter (\pos -> isEmpty $ b ! pos) . filter inRange
    validTake  = filter (\pos -> isEnemy $ b ! pos) . filter inRange
    validEmptyOrTake =
        filter (\pos -> isEmpty (b ! pos) || isEnemy (b ! pos)) . filter inRange
    inRange (x, y) = x >= 1 && x <= 8 && y >= 1 && y <= 8
    isEmpty (Just _) = False
    isEmpty Nothing  = True
    isMine (Just (player', _)) = player' == player
    isMine Nothing             = False
    isEnemy (Just (player', _)) = player' == otherPlayer player
    isEnemy Nothing             = False
    allPosInDirection mult (xDir, yDir)
        | not (inRange newPos) = []
        | isEnemy dest         = [newPos]
        | isMine dest          = []
        | otherwise = newPos : allPosInDirection (mult + 1) (xDir, yDir)
      where
        newPos = (x + mult * xDir, y + mult * yDir)
        dest   = b ! newPos
    makeMove oldpos newpos newpiece =
        let b1 = setElem Nothing oldpos b
            b2 = setElem (Just (player, newpiece)) newpos b1
        in  Board b2
    makeGame b = Game { gamePlayer = otherPlayer player, gameBoard = b }


otherPlayer :: Player -> Player
otherPlayer Black = White
otherPlayer White = Black
