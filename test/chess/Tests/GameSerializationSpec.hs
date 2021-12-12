module Tests.GameSerializationSpec where

import           Chess                                    ( Game(..)
                                                          , defaultGame
                                                          )
import           Test.Hspec                               ( Spec(..)
                                                          , it
                                                          , shouldBe
                                                          )

spec :: Spec
spec = do
    it "can serialize and deserialize into the same game state" $ do
        let game = defaultGame
        game `shouldBe` (read . show $ game)
