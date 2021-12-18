-- | Common minimax definitions

module Minimax.Common
  ( Depth
  ) where

-- data MinimaxTree = MinimaxNode Int Board [MinimaxTree] | MinimaxLeaf Int Board
type Depth = Int

-- getTreeVal :: MinimaxTree -> Int
-- getTreeVal tree = case tree of
--     MinimaxNode x _ _ -> x
--     MinimaxLeaf x _ -> x

-- getTreeBoard :: MinimaxTree -> Board
-- getTreeBoard tree = case tree of
--   MinimaxNode _ b _ -> b
--   MinimaxLeaf _ b -> b
