module Tests.BoardScoreSpec where

import           Chess                                    ( Board
                                                          , boardScore
                                                          , defaultBoard
                                                          )
import           Test.Hspec                               ( Spec(..)
                                                          , it
                                                          , shouldBe
                                                          )

spec :: Spec
spec = do
    it "default board should have score 0" $ do
        let board = defaultBoard
        boardScore board `shouldBe` 0
