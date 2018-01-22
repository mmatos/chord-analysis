module TestHelpers where
import Test.Hspec
import Notes

shouldHaveNotes withNotes noteStrings = (notes withNotes) `shouldBe` (readNotes noteStrings)
shouldShowLike showable string = (show showable) `shouldBe` string