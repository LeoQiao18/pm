-- | Chess representations

module Chess
    ( Board(..)
    , Piece(..)
    , Position
    , Player(..)
    , boardFromFile
    , initialBoard
    ) where

data Player = Black | White deriving (Show, Eq)

data Board = Board Player [[(Player, Piece)]]
    deriving (Show, Eq)

data Piece =
   Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Show, Eq)

type Position = (Int, Int)

boardFromFile :: String -> Board
boardFromFile = undefined

initialBoard :: Board
initialBoard = undefined
