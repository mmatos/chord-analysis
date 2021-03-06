import ChordAnalysis
import Chords
import Notes
import Scales
import Test.Hspec

main = hspec $ do
  describe "Nth Chord functions in a scale" $ do
    it "V chord for C Ionian" $ do
      nthChord 5 (Modal Ionian (N C Nat)) `shouldBe` (majorChord (N G Nat))
    it "VII chord for F Ionian" $ do
      nthChord 7 (Modal Ionian (N F Nat)) `shouldBe` (dimChord (N E Nat))
    it "II chord for D Dorian" $ do
      nthChord 2 (Modal Dorian (N D Nat)) `shouldBe` (minorChord (N E Nat))
    it "I chord for F Dorian" $ do
      nthChord 1 (Modal Dorian (N F Nat)) `shouldBe` (minorChord (N F Nat))
    it "VII chord for A Aeolian" $ do
      nthChord 6 (Modal Aeolian (N A Nat)) `shouldBe` (majorChord (N F Nat))
    it "IV chord for F Aeolian" $ do
      nthChord 4 (Modal Aeolian (N F Nat)) `shouldBe` (minorChord (N B Flat))
    it "III chord for C Mixolydian" $ do
      nthChord 3 (Modal Mixolydian (N C Nat)) `shouldBe` (dimChord (N E Nat))

  describe "Applied Nth Chord functions in a scale" $ do
    it "V/V chord for C Ionian" $ do
      applied 5 5 (Modal Ionian (N C Nat)) `shouldBe` (majorChord (N D Nat))
    it "V/VI chord for F Ionian" $ do
      applied 5 4 (Modal Ionian (N F Nat)) `shouldBe` (majorChord (N F Nat))
    it "IV/II chord for D Dorian" $ do
      applied 4 2 (Modal Dorian (N D Nat)) `shouldBe` (majorChord (N A Nat))
    it "IV/III chord for F Dorian" $ do
      applied 4 3 (Modal Dorian (N F Nat)) `shouldBe` (majorChord (N D Flat))
    it "II/V chord for A Aeolian" $ do
      applied 2 5 (Modal Aeolian (N A Nat)) `shouldBe` (dimChord (N F Sharp))
    it "VII/V chord for F Aeolian" $ do
      applied 7 5 (Modal Aeolian (N F Nat)) `shouldBe` (majorChord (N B Flat))
    it "I/V chord for C Mixolydian" $ do
      applied 1 5 (Modal Mixolydian (N C Nat)) `shouldBe` (majorChord (N G Nat))

  describe "Borrowed Nth Chord from other mode functions in a scale" $ do
    it "V Aeolian chord for C Ionian" $ do
      borrow 5 Aeolian (Modal Ionian (N C Nat)) `shouldBe` (minorChord (N G Nat))
    it "V Phrygian chord for F Ionian" $ do
      borrow 5 Phrygian (Modal Ionian (N F Nat)) `shouldBe` (dimChord (N C Nat))
    it "II Aeolian chord for C Ionian" $ do
      borrow 2 Aeolian (Modal Ionian (N C Nat)) `shouldBe` (dimChord (N D Nat))
    it "VII Aeolian chord for F Ionian" $ do
      borrow 7 Aeolian (Modal Ionian (N F Nat)) `shouldBe` (majorChord (N E Flat))
    it "VII Phrygian chord for F Ionian" $ do
      borrow 7 Phrygian (Modal Ionian (N F Nat)) `shouldBe` (minorChord (N E Flat))
    it "III Lydian chord for C Mixolydian" $ do
      borrow 3 Lydian (Modal Mixolydian (N C Nat)) `shouldBe` (minorChord (N E Nat))
    it "IV Lydian chord for C Mixolydian" $ do
      borrow 4 Lydian (Modal Mixolydian (N C Nat)) `shouldBe` (dimChord (N F Sharp))
    it "II Lydian chord for D Dorian" $ do
      borrow 2 Lydian (Modal Dorian (N D Nat)) `shouldBe` (majorChord (N E Nat))
    it "VI Lydian chord for D Locrian" $ do
      borrow 6 Lydian (Modal Locrian (N D Nat)) `shouldBe` (minorChord (N B Nat))