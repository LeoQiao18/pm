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
    -- TODO: more test cases
  let game = defaultGame
  it "Black King should not be able to move in default game position" $ do
    legalMovesForPos game (1, 5) `shouldBe` []
  it "Black Pawn can move two steps in default game position" $ do
    let game'   = setBoardPiece game (2, 1) Nothing
        game''  = setBoardPiece game' (4, 1) (Just (Black, Pawn))
        game''' = setPlayer game'' White
    legalMoves game `shouldContain` [game''']
  -- it "can generate next moves for a Knight" $ do
  --     let piece = b `atPos` (1, 2)
  --     piece `shouldBe` Just (Black, Knight)
  --     getAllNextPos b (1, 2) piece `shouldBe` [(2, 4), (3, 3), (3, 1)]
  -- it "can generate next moves for a Rook (blocked)" $ do
  --     let piece = b `atPos` (1, 1)
  --     piece `shouldBe` Just (Black, Rook)
  --     getAllNextPos b (1, 1) piece `shouldBe` []
  -- let b = testBoard1
  -- it "can generate next moves for a Rook (unblocked)" $ do
  --     let piece = b `atPos` (1, 1)
  --     piece `shouldBe` Just (Black, Rook)
  --     getAllNextPos b (1, 1) piece
  --         `shouldBe` [ (2, 1)
  --                    , (3, 1)
  --                    , (4, 1)
  --                    , (5, 1)
  --                    , (6, 1)
  --                    , (7, 1)
  --                    , (8, 1)
  --                    , (1, 2)
  --                    ]
  -- it "can generate next moves for a pawn" $ do
  --     let piece = b `atPos` (1, 4)
  --     piece `shouldBe` Just (Black, Pawn)
  --     getAllNextPos b (1, 4) piece `shouldBe` [(2, 4), (2, 3)]
  -- it "can generate next moves for a queen" $ do
  --     let piece = b `atPos` (1, 3)
  --     piece `shouldBe` Just (Black, Queen)
  --     getAllNextPos b (1, 3) piece
  --         `shouldBe` [ (2, 3)
  --                    , (1, 2)
  --                    , (2, 4)
  --                    , (3, 5)
  --                    , (4, 6)
  --                    , (5, 7)
  --                    , (6, 8)
  --                    , (2, 2)
  --                    , (3, 1)
  --                    ]

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
