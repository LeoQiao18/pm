module Tests.MoveGenerationSpec where

import           Chess                                    ( Game(..)
                                                          , Piece(..)
                                                          , Player(..)
                                                          , atPos
                                                          , board
                                                          , defaultGame
                                                          , getBoardMatrix
                                                          , setBoardPiece
                                                          , setPlayer
                                                          )
import           Rules                                    ( legalMoves
                                                          , legalMovesForPos
                                                          )
import           Test.Hspec                               ( Spec(..)
                                                          , it
                                                          , shouldBe
                                                          , shouldContain
                                                          , shouldSatisfy
                                                          )


spec :: Spec
spec = do
  let game = defaultGame
  it "Black King should not be able to move in default game position" $ do
    legalMovesForPos game (1, 5) `shouldBe` []
  it "Black Pawn can move two steps in default game position" $ do
    let game'   = setBoardPiece game (2, 1) Nothing
        game''  = setBoardPiece game' (4, 1) (Just (Black, Pawn))
        game''' = setPlayer game'' White
    legalMoves game `shouldContain` [game''']

testBoard1 = board b
 where
  b =
    [ [ Just (Black, Rook)
      , Nothing
      , Just (Black, Queen)
      , Just (Black, Pawn)
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just (White, Knight)
      , Nothing
      , Just (Black, Knight)
      , Nothing
      , Nothing
      , Nothing
      ]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    ]
