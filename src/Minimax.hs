-- | Common minimax definitions

module Minimax
    ( MinimaxTree(..)
    , Depth
    ) where

import           Chess                                    ( Board(..) )

data MinimaxTree = MinimaxNode Int Board [MinimaxTree] | MinimaxLeaf Int Board
type Depth = Int
