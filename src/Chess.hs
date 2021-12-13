-- | Chess representations

module Chess
  ( Board(..)
  , board
  , Game(..)
  , Piece(..)
  , Position
  , Player(..)
  , atPos
  , prettyGame
  , defaultGame
  , defaultBoard
  , gameScore
  , boardScore
  , positionScore
  , boardPieceScore
  , pieceScore
  , positionMultiplier
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

type Score = Int

type Multiplier = Int

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
-- evaluateBoard :: Board -> Score
-- evaluateBoard (Board b) = foldl (foldl addPieceScore) 0 b
--   where addPieceScore score Nothing = score
--         addPieceScore score (Just (Black, p)) =
--         addPieceScore score (Just (White, p)) =




-- score
gameScore :: Game -> Score
gameScore g = boardScore $ gameBoard g

boardScore :: Board -> Score
boardScore (Board b) = foldl
  (\score pos -> score + positionScore (b ! pos) pos)
  0
  indicies
  where indicies = [ (r, c) | r <- [1 .. 8], c <- [1 .. 8] ]

positionScore :: BoardPiece -> Position -> Score
positionScore bp pos = multiplier * score
 where
  multiplier = positionMultiplier bp pos
  score      = boardPieceScore bp

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

-- positionMultiplier
positionMultiplier :: BoardPiece -> Position -> Multiplier
positionMultiplier Nothing           = const 0
positionMultiplier (Just (Black, p)) = const 1
positionMultiplier (Just (White, p)) = const 1
