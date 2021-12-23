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

spec :: Spec
spec = do
  it "initial game should have 0 score" $ do
    gameScore defaultGame `shouldBe` 0
