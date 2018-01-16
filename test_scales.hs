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
      notes (Modal Ionian (N C Nat)) `shouldBe` (map (flip N Nat) [C, D, E, F, G, A, B, C])
    it "D Dorian has all natural notes" $ do
      notes (Modal Dorian (N D Nat)) `shouldBe` (map (flip N Nat) [D, E, F, G, A, B, C, D])
    it "E Phrygian has all natural notes" $ do
      notes (Modal Phrygian (N E Nat)) `shouldBe` (map (flip N Nat) [E, F, G, A, B, C, D, E])
    it "F Lydian has all natural notes" $ do
      notes (Modal Lydian (N F Nat)) `shouldBe` (map (flip N Nat) [F, G, A, B, C, D, E, F])
    it "G Mixolydian has all natural notes" $ do
      notes (Modal Mixolydian (N G Nat)) `shouldBe` (map (flip N Nat) [G, A, B, C, D, E, F, G])
    it "A Aeolian has all natural notes" $ do
      notes (Modal Aeolian (N A Nat)) `shouldBe` (map (flip N Nat) [A, B, C, D, E, F, G, A])
    it "B Locrian has all natural notes" $ do
      notes (Modal Locrian (N B Nat)) `shouldBe` (map (flip N Nat) [B, C, D, E, F, G, A, B])
    it "F Ionian has flats" $ do
      notes (Modal Ionian (N F Nat)) `shouldBe` [N F Nat, N G Nat, N A Nat, N B Flat, N C Nat, N D Nat, N E Nat, N F Nat]
    it "B Aeolian has sharps" $ do
      notes (Modal Aeolian (N B Nat)) `shouldBe` [N B Nat, N C Sharp, N D Nat, N E Nat, N F Sharp, N G Nat, N A Nat, N B Nat]
    it "F# Aeolian has F#, C# and G#" $ do
      notes (Modal Aeolian (N F Sharp)) `shouldBe` [N F Sharp, N G Sharp, N A Nat, N B Nat, N C Sharp, N D Nat, N E Nat, N F Sharp]
    it "Bb Dorian has Bb, Eb, Ab and Db" $ do
      notes (Modal Dorian (N B Flat)) `shouldBe` [N B Flat, N C Nat, N D Flat, N E Flat, N F Nat, N G Nat, N A Flat, N B Flat]
