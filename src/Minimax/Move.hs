module Minimax.Move
    ( PMStrategy(..)
    , bestMove
    ) where

import           Chess                                    ( Game(..) )
import           Minimax.Common                           ( Depth )
import qualified Minimax.Par.Move              as P
import qualified Minimax.Seq.Move              as S
import qualified Minimax.SeqAB.Move            as SAB

data PMStrategy
  = MinimaxSeq Depth
  | MinimaxPar Depth Depth -- parDepth, depth
  | MinimaxSeqAB Depth
  deriving (Read, Show, Eq)


bestMove :: PMStrategy -> Game -> Game
bestMove pmStrat g = case pmStrat of
    MinimaxSeq depth          -> S.bestMove depth g
    MinimaxPar parDepth depth -> P.bestMove parDepth depth g
    MinimaxSeqAB depth        -> SAB.bestMove depth g
