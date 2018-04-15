import ChordAnalysis
import Chords
import Notes
import Scales
import Test.Hspec

shouldInclude actualValues minimumExpectedValues
  = all (flip elem actualValues) minimumExpectedValues `shouldBe` True
shouldNotInclude actualValues unexpectedValue
  = elem unexpectedValue actualValues `shouldBe` False

main = hspec $ do
  describe "Analyze which possible functions does a chord play in a scale" $ do
    it "Possible functions for D for C Ionian" $ do
      analyzeChord (majorChord (N D Nat)) (majorScale (N C Nat)) `shouldInclude` ["IV/VI","V/V","II Lydian"]
    it "Possible functions for C#- for D Dorian" $ do
      analyzeChord (minorChord (N C Sharp)) (Modal Dorian (N D Nat)) `shouldInclude` ["II/VI","VII Lydian"]
    it "A chord from the progression should play an nth function" $ do
      analyzeChord (minorChord (N E Nat)) (majorScale (N C Nat)) `shouldInclude` ["III"]
    it "NTH/I is not a valid function" $ do
        analyzeChord (minorChord (N E Nat)) (majorScale (N C Nat)) `shouldNotInclude` "III/I"
