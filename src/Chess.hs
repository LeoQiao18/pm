-- | Chess representations

module Chess
  ( Board(..)
  , Piece(..)
  , Position
  , position
  , Player(..)
  , getPieceAtPosition
  , prettyBoard
  , defaultBoard
  ) where
import           Data.Char                                ( toLower )

data Player = Black | White deriving (Read, Show, Eq)

data Board = Board Player [[Maybe (Player, Piece)]]
  deriving (Read, Show, Eq)

data Piece =
   Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Read, Show, Eq)

data Position = Position Int Int

position r c = if isValidPosition
  then Position r c
  else error $ "Invalid position: " ++ show (r, c)
 where
  isValidPosition | (0 <= r) && (r < 8) && (0 <= c) && (c < 8) = True
                  | otherwise = False

getPieceAtPosition :: Board -> Position -> Maybe (Player, Piece)
getPieceAtPosition (Board _ board) (Position r c) = board !! r !! c

evaluateBoard :: Board -> Int
evaluateBoard = undefined

prettyBoard :: Board -> String
prettyBoard (Board player board) =
  "> Player: "
    ++ show player
    ++ "\n"
    ++ concatMap (\row -> "|" ++ prettyRow row ++ "|\n") board
 where
  prettyRow (p1 : p2 : ps) = prettyPosition p1 ++ "|" ++ prettyRow (p2 : ps)
  prettyRow (p1      : ps) = prettyPosition p1
  prettyRow []             = error "Can't pretty print an empty board row"
  prettyPosition Nothing    = "  "
  prettyPosition (Just pos) = prettyPiece pos
  prettyPiece (player, piece) =
    let player' = toLower . head . show $ player
        piece'  = toLower . head . show $ piece
    in  [player', piece']

defaultBoard :: Board
defaultBoard = Board Black board'
 where
  board' =
    [ [ Just (Black, Rook)
      , Just (Black, Knight)
      , Just (Black, Bishop)
      , Just (Black, Queen)
      , Just (Black, King)
      , Just (Black, Bishop)
      , Just (Black, Knight)
      , Just (Black, Rook)
      ]
    , [ Just (Black, Pawn)
      , Just (Black, Pawn)
      , Just (Black, Pawn)
      , Just (Black, Pawn)
      , Just (Black, Pawn)
      , Just (Black, Pawn)
      , Just (Black, Pawn)
      , Just (Black, Pawn)
      ]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [ Just (White, Pawn)
      , Just (White, Pawn)
      , Just (White, Pawn)
      , Just (White, Pawn)
      , Just (White, Pawn)
      , Just (White, Pawn)
      , Just (White, Pawn)
      , Just (White, Pawn)
      ]
    , [ Just (White, Rook)
      , Just (White, Knight)
      , Just (White, Bishop)
      , Just (White, Queen)
      , Just (White, King)
      , Just (White, Bishop)
      , Just (White, Knight)
      , Just (White, Rook)
      ]
    ]
