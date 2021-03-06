import Scales
import Notes
import Test.Hspec
import TestHelpers

main = hspec $ do

  describe "Major and Minor scales" $ do
    it "An Ionian scale is major" $ do
      isMajor (Modal Ionian (N C Nat)) `shouldBe` True
    it "A Dorian scale is minor" $ do
      isMinor (Modal Dorian (N C Nat)) `shouldBe` True
    it "A Phrygian scale is minor" $ do
      isMinor (Modal Phrygian (N C Nat)) `shouldBe` True
    it "A Lydian scale is major" $ do
      isMajor (Modal Lydian (N C Nat)) `shouldBe` True
    it "A Mixolydian scale is major" $ do
      isMajor (Modal Mixolydian (N C Nat)) `shouldBe` True
    it "An Aeolian scale is minor" $ do
      isMinor (Modal Aeolian (N C Nat)) `shouldBe` True
    it "A Locrian scale is major" $ do
      isMinor (Modal Locrian (N C Nat)) `shouldBe` True

  describe "Natural notes for each mode" $ do
    it "C is natural for Ionian" $ do
      naturalNote Ionian `shouldBe` (N C Nat)
    it "D is natural for Dorian" $ do
      naturalNote Dorian `shouldBe` (N D Nat)
    it "E is natural for Phrygian" $ do
      naturalNote Phrygian `shouldBe` (N E Nat)
    it "F is natural for Lydian" $ do
      naturalNote Lydian `shouldBe` (N F Nat)
    it "G is natural for Mixolydian" $ do
      naturalNote Mixolydian `shouldBe` (N G Nat)
    it "A is natural for Aeolian" $ do
      naturalNote Aeolian `shouldBe` (N A Nat)
    it "B is natural for Locrian" $ do
      naturalNote Locrian `shouldBe` (N B Nat)

  describe "Notes in a scale" $ do
    it "C Ionian has all natural notes" $ do
      Modal Ionian (N C Nat) `shouldHaveNotes` ["C", "D", "E", "F", "G", "A", "B"]
    it "D Dorian has all natural notes" $ do
      Modal Dorian (N D Nat) `shouldHaveNotes` ["D", "E", "F", "G", "A", "B", "C"]
    it "E Phrygian has all natural notes" $ do
      Modal Phrygian (N E Nat) `shouldHaveNotes`  ["E", "F", "G", "A", "B", "C", "D"]
    it "F Lydian has all natural notes" $ do
      Modal Lydian (N F Nat) `shouldHaveNotes`  ["F", "G", "A", "B", "C", "D", "E"]
    it "G Mixolydian has all natural notes" $ do
      Modal Mixolydian (N G Nat) `shouldHaveNotes` ["G", "A", "B", "C", "D", "E", "F"]
    it "A Aeolian has all natural notes" $ do
      Modal Aeolian (N A Nat) `shouldHaveNotes`  ["A", "B", "C", "D", "E", "F", "G"]
    it "B Locrian has all natural notes" $ do
      Modal Locrian (N B Nat) `shouldHaveNotes`  ["B", "C", "D", "E", "F", "G", "A"]
    it "F Ionian has flats" $ do
      Modal Ionian (N F Nat) `shouldHaveNotes`  ["F", "G", "A", "Bb", "C", "D", "E"]
    it "B Aeolian has sharps" $ do
      Modal Aeolian (N B Nat) `shouldHaveNotes`  ["B", "C#", "D", "E", "F#", "G", "A"]
    it "F# Aeolian has F#, C# and G#" $ do
      Modal Aeolian (N F Sharp) `shouldHaveNotes` ["F#", "G#", "A", "B", "C#", "D", "E"]
    it "Bb Dorian has Bb, Eb, Ab and Db" $ do
      Modal Dorian (N B Flat) `shouldHaveNotes` ["Bb", "C", "Db", "Eb", "F", "G", "Ab"]
