module Tests.MinimaxLogicSpec where

import           Chess                                    ( Game(..)
                                                          , Player(..)
                                                          , Piece(..)
                                                          , board
                                                          , defaultGame
                                                          , atPos, defaultBoard
                                                          )
import           Test.Hspec                               ( Spec(..)
                                                          , it
                                                          , shouldBe
                                                          )                                           
import           Seq.Move                                 ( evaluateBoard
                                                          , getNextMove
                                                          )

testBoard1 = board b
 where b =
        [ [ Just (Black, Rook)
        , Nothing
        , Just (Black, Queen)
        , Just (Black, Pawn)
        , Nothing, Nothing, Nothing, Nothing
        ]
        , [ Nothing
        , Nothing
        , Just (White, Knight)
        , Nothing
        , Just (Black, Knight)
        , Nothing, Nothing, Nothing
        ]
        , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
        ]

spec :: Spec
spec = do
    it "can correctly evaluate a board" $ do
        evaluateBoard defaultBoard Black `shouldBe` 0
        evaluateBoard defaultBoard White `shouldBe` 0
        evaluateBoard testBoard1 Black `shouldBe` 150
        evaluateBoard testBoard1 White `shouldBe` (-150)