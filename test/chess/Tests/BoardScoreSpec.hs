module Tests.BoardScoreSpec where

import           Chess                                    ( Game(..)
                                                          , Piece(..)
                                                          , Player(..)
                                                          , board
                                                          , defaultBoard
                                                          , defaultGame
                                                          )
import           Score                                    ( gameScore )
import           Test.Hspec                               ( Spec(..)
                                                          , it
                                                          , shouldBe
                                                          )

-- testBoard1 = board b
--  where
--   b =
--     [ [ Just (Black, Rook)
--       , Nothing
--       , Just (Black, Queen)
--       , Just (Black, Pawn)
--       , Nothing
--       , Nothing
--       , Nothing
--       , Nothing
--       ]
--     , [ Nothing
--       , Nothing
--       , Just (White, Knight)
--       , Nothing
--       , Just (Black, Knight)
--       , Nothing
--       , Nothing
--       , Nothing
--       ]
--     , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
--     , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
--     , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
--     , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
--     , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
--     , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
--     ]

spec :: Spec
spec = do
  it "initial game should have 0 score" $ do
    gameScore defaultGame `shouldBe` 0
