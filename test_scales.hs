import Scales
import Notes
import Test.Hspec

main = hspec $ do
  describe "Major and Minor modes" $ do
    it "Ionian is major" $ do
      isMajorMode Ionian `shouldBe` True
    it "Dorian is minor" $ do
      isMinorMode Dorian `shouldBe` True
    it "Phrygian is minor" $ do
      isMinorMode Phrygian `shouldBe` True
    it "Lydian is major" $ do
      isMajorMode Lydian `shouldBe` True
    it "Mixolydian is major" $ do
      isMajorMode Mixolydian `shouldBe` True
    it "Aeolian is minor" $ do
      isMinorMode Aeolian `shouldBe` True
    it "Locrian is major" $ do
      isMinorMode Locrian `shouldBe` True

  describe "Natural notes for each mode" $ do
    it "C is natural for Ionian" $ do
      naturalNote Ionian `shouldBe` (readNote "C")
    it "D is natural for Dorian" $ do
      naturalNote Dorian `shouldBe` (readNote "D")
    it "E is natural for Phrygian" $ do
      naturalNote Phrygian `shouldBe` (readNote "E")
    it "F is natural for Lydian" $ do
      naturalNote Lydian `shouldBe` (readNote "F")
    it "G is natural for Mixolydian" $ do
      naturalNote Mixolydian `shouldBe` (readNote "G")
    it "A is natural for Aeolian" $ do
      naturalNote Aeolian `shouldBe` (readNote "A")
    it "B is natural for Locrian" $ do
      naturalNote Locrian `shouldBe` (readNote "B")

  describe "Notes in a scale" $ do
    it "C Ionian has all natural notes" $ do
      notes (Modal Ionian (N C Nat)) `shouldBe` (map readNote ["C", "D", "E", "F", "G", "A", "B", "C"])
    it "D Dorian has all natural notes" $ do
      notes (Modal Dorian (N D Nat)) `shouldBe` (map readNote ["D", "E", "F", "G", "A", "B", "C", "D"])
    it "E Phrygian has all natural notes" $ do
      notes (Modal Phrygian (N E Nat)) `shouldBe` (map readNote ["E", "F", "G", "A", "B", "C", "D", "E"])
    it "F Lydian has all natural notes" $ do
      notes (Modal Lydian (N F Nat)) `shouldBe` (map readNote ["F", "G", "A", "B", "C", "D", "E", "F"])
    it "G Mixolydian has all natural notes" $ do
      notes (Modal Mixolydian (N G Nat)) `shouldBe` (map readNote ["G", "A", "B", "C", "D", "E", "F", "G"])
    it "A Aeolian has all natural notes" $ do
      notes (Modal Aeolian (N A Nat)) `shouldBe` (map readNote ["A", "B", "C", "D", "E", "F", "G", "A"])
    it "B Locrian has all natural notes" $ do
      notes (Modal Locrian (N B Nat)) `shouldBe` (map readNote ["B", "C", "D", "E", "F", "G", "A", "B"])
    it "F Ionian has flats" $ do
      notes (Modal Ionian (N F Nat)) `shouldBe` (map readNote ["F", "G", "A", "Bb", "C", "D", "E", "F"])
    it "B Aeolian has sharps" $ do
      notes (Modal Aeolian (N B Nat)) `shouldBe` (map readNote ["B", "C#", "D", "E", "F#", "G", "A", "B"])
    it "F# Aeolian has F#, C# and G#" $ do
      notes (Modal Aeolian (N F Sharp)) `shouldBe` (map readNote ["F#", "G#", "A", "B", "C#", "D", "E", "F#"])
    it "Bb Dorian has Bb, Eb, Ab and Db" $ do
      notes (Modal Dorian (N B Flat)) `shouldBe` (map readNote ["Bb", "C", "Db", "Eb", "F", "G", "Ab", "Bb"])
