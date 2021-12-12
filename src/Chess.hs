-- | Chess representations

module Chess
  ( Board
  , board
  , Game(..)
  , Piece(..)
  , Position
  , Player(..)
  , atPos
  , prettyGame
  , defaultGame
  , defaultBoard
  ) where
import           Data.Char                                ( toLower )

data Game = Game
  { gamePlayer :: Player
  , gameBoard  :: Board
  }
  deriving (Read, Show, Eq)

data Player = Black | White deriving (Read, Show, Eq)

newtype Board = Board [[Maybe (Player, Piece)]] deriving (Read, Show, Eq)

-- Board data constructor is internal;
-- this board function does dimension check on the 2D list
board :: [[Maybe (Player, Piece)]] -> Board
board b = if isValidBoard
  then Board b
  else error "Dimension of board is not 8*8"
 where
  validNumOfRows    = length b == 8
  validNumOfColumns = all (\row -> length row == 8) b
  isValidBoard      = validNumOfRows && validNumOfColumns

data Piece =
   Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Read, Show, Eq)

type Position = (Int, Int)

-- atPos also checks index range to be within 8*8
atPos :: Board -> Position -> Maybe (Player, Piece)
atPos (Board b) pos@(r, c) = if isValidPosition
  then b !! r !! c
  else error $ "Invalid position: " ++ show pos
 where
  isValidPosition | (0 <= r) && (r < 8) && (0 <= c) && (c < 8) = True
                  | otherwise = False

evaluateBoard :: Board -> Int
evaluateBoard = undefined

-- pretty print Game
prettyGame :: Game -> String
prettyGame g = "> Player: " ++ show (gamePlayer g) ++ "\n" ++ prettyBoard
  (gameBoard g)
 where
  prettyBoard (Board b) = concatMap (\row -> "|" ++ prettyRow row ++ "|\n") b
  prettyRow (p1 : p2 : ps) = prettyPosition p1 ++ "|" ++ prettyRow (p2 : ps)
  prettyRow (p1      : ps) = prettyPosition p1
  prettyRow []             = error "Can't pretty print an empty board row"
  prettyPosition Nothing    = "  "
  prettyPosition (Just pos) = prettyPiece pos
  prettyPiece (player, piece) =
    let player' = toLower . head . show $ player
        piece'  = toLower . head . show $ piece
    in  [player', piece']

-- default start game state
defaultGame :: Game
defaultGame = Game { gamePlayer = Black, gameBoard = defaultBoard }

defaultBoard :: Board
defaultBoard = board b
 where
  b =
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
