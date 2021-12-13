-- | Common minimax definitions

module Minimax
    ( MinimaxTree(..)
    , Depth
    ) where

import           Chess                                    ( Game(..) )

data MinimaxTree = MinimaxNode Int Game [MinimaxTree] | MinimaxLeaf Int Game
type Depth = Int
