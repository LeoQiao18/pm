-- | Chess representations

module Chess
  ( Board(..)
  , board
  , BoardPiece(..)
  , Game(..)
  , Piece(..)
  , Position
  , Player(..)
  , atPos
  , prettyGame
  , defaultGame
  , defaultBoard
  ) where
import           Data.Bifunctor                           ( first )
import           Data.Char                                ( toLower )
import           Data.List                                ( intercalate )
import           Data.Matrix                              ( (!)
                                                          , Matrix
                                                          , fromLists
                                                          , matrix
                                                          , toLists
                                                          )

data Game = Game
  { gamePlayer :: Player
  , gameBoard  :: Board
  }
  deriving (Read, Show, Eq)

data Player = Black | White deriving (Read, Show, Eq)

newtype Board = Board (Matrix BoardPiece) deriving Eq

type BoardPiece = Maybe (Player, Piece)

instance Show Board where
  show (Board b) = show $ toLists b

instance Read Board where
  readsPrec prec s = map (first (Board . fromLists)) (readsPrec prec s)

-- checks dimension on the 2D list
board :: [[BoardPiece]] -> Board
board b = if isValidBoard
  then Board $ fromLists b
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

-- unsafe: get piece at position
atPos :: Board -> Position -> BoardPiece
atPos (Board b) pos = b ! pos

-- pretty print Game
prettyGame :: Game -> String
prettyGame g = "> Player: " ++ show (gamePlayer g) ++ "\n" ++ prettyBoard
  (gameBoard g)
 where
  prettyBoard (Board b) =
    intercalate "\n" . map prettyRow . toLists $ fmap prettyBoardPiece b
  prettyRow row = "|" ++ intercalate "|" row ++ "|"
  prettyBoardPiece Nothing  = "  "
  prettyBoardPiece (Just p) = prettyPiece p
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

-- board evaluation
-- evaluateBoard :: Board -> Int
-- evaluateBoard (Board b) = foldl (foldl addPieceScore) 0 b
--   where addPieceScore score Nothing = score
--         addPieceScore score (Just (Black, p)) =
--         addPieceScore score (Just (White, p)) =


-- pieceFactorMap :: Piece
