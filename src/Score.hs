-- | Chess board evaluation

module Score
    ( gameScore
    , boardScore
    , Score
    ) where
import           Chess                                    ( Board(..)
                                                          , BoardPiece(..)
                                                          , Game(..)
                                                          , Piece(..)
                                                          , Player(..)
                                                          , Position(..)
                                                          )
import           Data.Matrix                              ( (!)
                                                          , Matrix(..)
                                                          , fromLists
                                                          , switchRows
                                                          )

type Score = Float

gameScore :: Game -> Score
gameScore g = boardScore $ gameBoard g

boardScore :: Board -> Score
boardScore (Board b) = foldl
    (\score pos -> score + positionScore (b ! pos) pos)
    0
    indicies
    where indicies = [ (r, c) | r <- [1 .. 8], c <- [1 .. 8] ]

positionScore :: BoardPiece -> Position -> Score
positionScore bp pos = score + bonus
  where
    score = boardPieceScore bp
    bonus = positionBonus bp pos

boardPieceScore :: BoardPiece -> Score
boardPieceScore Nothing           = 0
boardPieceScore (Just (Black, p)) = -1 * pieceScore p
boardPieceScore (Just (White, p)) = pieceScore p

pieceScore :: Piece -> Score
pieceScore Pawn   = 10
pieceScore Knight = 30
pieceScore Bishop = 30
pieceScore Rook   = 50
pieceScore Queen  = 90
pieceScore King   = 900

positionBonus :: BoardPiece -> Position -> Score
positionBonus Nothing                _   = 0
positionBonus (Just (player, piece)) pos = bonusMap player piece ! pos

bonusMap :: Player -> Piece -> Matrix Score
bonusMap White piece = pieceBonusMap piece
bonusMap Black piece = fmap negate $ reflectOverX $ pieceBonusMap piece
  where
    reflectOverX mat =
        switchRows 1 8 $ switchRows 2 7 $ switchRows 3 6 $ switchRows 4 5 mat
pieceBonusMap Pawn = fromLists
    [ [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    , [5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0]
    , [1.0, 1.0, 2.0, 3.0, 3.0, 2.0, 1.0, 1.0]
    , [0.5, 0.5, 1.0, 2.5, 2.5, 1.0, 0.5, 0.5]
    , [0.0, 0.0, 0.0, 2.0, 2.0, 0.0, 0.0, 0.0]
    , [0.5, -0.5, -1.0, 0.0, 0.0, -1.0, -0.5, 0.5]
    , [0.5, 1.0, 1.0, -2.0, -2.0, 1.0, 1.0, 0.5]
    , [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    ]
pieceBonusMap Knight = fromLists
    [ [-5.0, -4.0, -3.0, -3.0, -3.0, -3.0, -4.0, -5.0]
    , [-4.0, -2.0, 0.0, 0.0, 0.0, 0.0, -2.0, -4.0]
    , [-3.0, 0.0, 1.0, 1.5, 1.5, 1.0, 0.0, -3.0]
    , [-3.0, 0.5, 1.5, 2.0, 2.0, 1.5, 0.5, -3.0]
    , [-3.0, 0.0, 1.5, 2.0, 2.0, 1.5, 0.0, -3.0]
    , [-3.0, 0.5, 1.0, 1.5, 1.5, 1.0, 0.5, -3.0]
    , [-4.0, -2.0, 0.0, 0.5, 0.5, 0.0, -2.0, -4.0]
    , [-5.0, -4.0, -3.0, -3.0, -3.0, -3.0, -4.0, -5.0]
    ]
pieceBonusMap Bishop = fromLists
    [ [-2.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -2.0]
    , [-1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0]
    , [-1.0, 0.0, 0.5, 1.0, 1.0, 0.5, 0.0, -1.0]
    , [-1.0, 0.5, 0.5, 1.0, 1.0, 0.5, 0.5, -1.0]
    , [-1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, -1.0]
    , [-1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, -1.0]
    , [-1.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.5, -1.0]
    , [-2.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -2.0]
    ]
pieceBonusMap Rook = fromLists
    [ [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    , [0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.5]
    , [-0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.5]
    , [-0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.5]
    , [-0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.5]
    , [-0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.5]
    , [-0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.5]
    , [0.0, 0.0, 0.0, 0.5, 0.5, 0.0, 0.0, 0.0]
    ]
pieceBonusMap Queen = fromLists
    [ [-2.0, -1.0, -1.0, -0.5, -0.5, -1.0, -1.0, -2.0]
    , [-1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0]
    , [-1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.0, -1.0]
    , [-0.5, 0.0, 0.5, 0.5, 0.5, 0.5, 0.0, -0.5]
    , [0.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.0, -0.5]
    , [-1.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, -1.0]
    , [-1.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, -1.0]
    , [-2.0, -1.0, -1.0, -0.5, -0.5, -1.0, -1.0, -2.0]
    ]
pieceBonusMap King = fromLists
    [ [-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0]
    , [-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0]
    , [-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0]
    , [-3.0, -4.0, -4.0, -5.0, -5.0, -4.0, -4.0, -3.0]
    , [-2.0, -3.0, -3.0, -4.0, -4.0, -3.0, -3.0, -2.0]
    , [-1.0, -2.0, -2.0, -2.0, -2.0, -2.0, -2.0, -1.0]
    , [2.0, 2.0, 0.0, 0.0, 0.0, 0.0, 2.0, 2.0]
    , [2.0, 3.0, 1.0, 0.0, 0.0, 1.0, 3.0, 2.0]
    ]
