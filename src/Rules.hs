-- | Chess rules

module Rules
    ( isGameOver
    , winner
    , legalMoves
    , legalMovesForPos
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
legalMovesForPos g@Game { gamePlayer = player, gameBoard = Board b } pos@(r, c)
    = case b ! pos of
        Nothing -> []
        Just (piecePlayer, piece) ->
            if piecePlayer == player then movesForPiece piece else []
  where
    movesForPiece Pawn = case player of
        Black ->
            let normalStep = validEmpty [(r + 1, c)]
                doubleStep = if r == 2 then validEmpty [(r + 2, c)] else []
                takes      = validTake [(r + 1, c - 1), (r + 1, c + 1)]
                poses      = normalStep ++ doubleStep ++ takes
                newBoards newpos@(r, c)
                    | r == 8
                    = let promotions = [Pawn, Knight, Bishop, Rook, Queen]
                      in  map (makeMove pos newpos) promotions
                    | otherwise
                    = [makeMove pos newpos Pawn]
            in  map makeGame $ concatMap newBoards poses
        White ->
            let normalStep = validEmpty [(r - 1, c)]
                doubleStep = if r == 7 then validEmpty [(r - 2, c)] else []
                takes      = validTake [(r - 1, c - 1), (r - 1, c + 1)]
                poses      = normalStep ++ doubleStep ++ takes
                newBoards newpos@(r, c)
                    | r == 1
                    = let promotions = [Pawn, Knight, Bishop, Rook, Queen]
                      in  map (makeMove pos newpos) promotions
                    | otherwise
                    = [makeMove pos newpos Pawn]
            in  map makeGame $ concatMap newBoards poses
    movesForPiece Knight =
        let poses = validEmptyOrTake
                [ (r + 1, c + 2)
                , (r + 1, c - 2)
                , (r + 2, c + 1)
                , (r + 2, c - 1)
                , (r - 1, c + 2)
                , (r - 1, c - 2)
                , (r - 2, c + 1)
                , (r - 2, c - 1)
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
                [ (r', c')
                | r' <- [(r - 1) .. (r + 1)]
                , c' <- [(c - 1) .. (c + 1)]
                , (r', c') /= (r, c)
                ]
        in  map (\newpos -> makeGame $ makeMove pos newpos King) poses
    validEmpty = filter (\pos -> isEmpty $ b ! pos) . filter inRange
    validTake  = filter (\pos -> isEnemy $ b ! pos) . filter inRange
    validEmptyOrTake =
        filter (\pos -> isEmpty (b ! pos) || isEnemy (b ! pos)) . filter inRange
    inRange (r, c) = r >= 1 && r <= 8 && c >= 1 && c <= 8
    isEmpty (Just _) = False
    isEmpty Nothing  = True
    isMine (Just (player', _)) = player' == player
    isMine Nothing             = False
    isEnemy (Just (player', _)) = player' == otherPlayer player
    isEnemy Nothing             = False
    allPosInDirection mult (rDir, cDir)
        | not (inRange newPos) = []
        | isEnemy dest         = [newPos]
        | isMine dest          = []
        | otherwise = newPos : allPosInDirection (mult + 1) (rDir, cDir)
      where
        newPos = (r + mult * rDir, c + mult * cDir)
        dest   = b ! newPos
    makeMove oldpos newpos newpiece =
        let b1 = setElem Nothing oldpos b
            b2 = setElem (Just (player, newpiece)) newpos b1
        in  Board b2
    makeGame b = Game { gamePlayer = otherPlayer player, gameBoard = b }


otherPlayer :: Player -> Player
otherPlayer Black = White
otherPlayer White = Black
