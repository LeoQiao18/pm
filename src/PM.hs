-- |

module PM where

-- board representation
data Board = Board Player [[(Player, Piece)]]

data Piece =
   Pawn
  | King

type Position = (Int, Int)

data Player = Black | White deriving (Show, Eq)

-- minimax tree representation
data MinimaxTree = MinimaxNode Int Board [MinimaxTree] | MinimaxLeaf Int Board
type Depth = Int

-- board initialization
boardFromFile :: String -> Board
boardFromFile = undefined

cleanBoard :: Board
cleanBoard = undefined

-- construct minimax tree
getNextMove :: Board -> Board
getNextMove = undefined

calculateTree :: Board -> Depth -> MinimaxTree
calculateTree = undefined

evaluateBoard :: Board -> Int
evaluateBoard = undefined

-- calculate moves
getNextMoves :: Board -> [Board]
getNextMoves = undefined

getNextMovesForPosition :: Board -> Position -> [Board]
getNextMovesForPosition = undefined
