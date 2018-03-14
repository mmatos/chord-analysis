import ChordAnalysis
import Chords
import Notes
import Scales
import Test.Hspec

shouldInclude actualValues minimumExpectedValues 
  = all (flip elem actualValues) minimumExpectedValues `shouldBe` True

main = hspec $ do
  describe "Analyze which possible functions does a chord play in a scale" $ do
    it "Possible functions for D for C Ionian" $ do
      analyzeChord (majorChord (N D Nat)) (Modal Ionian (N C Nat)) `shouldInclude` ["IV/VI","V/V","II Lydian"]
    it "Possible functions for C#- for D Dorian" $ do
      analyzeChord (minorChord (N C Sharp)) (Modal Dorian (N D Nat)) `shouldInclude` ["II/VI","VII Lydian"]
