import Notes
import Test.Hspec
import TestHelpers

main = hspec $ do
  describe "Note printing" $ do
    it "Print C natural" $ do
      (N C Nat) `shouldShowLike` "C"
    it "Print B flat" $ do
      (N B Flat) `shouldShowLike` "Bb"
    it "Print F sharp" $ do
      (N F Sharp) `shouldShowLike` "F#"

  describe "Note reading" $ do
    it "Read C#" $ do
      readNote "C#" `shouldBe` (N C Sharp)
    it "Read Eb" $ do
      readNote "Eb" `shouldBe` (N E Flat)
    it "Read A" $ do
      readNote "A" `shouldBe` (N A Nat)

  describe "Notes with the same numeric value" $ do
    it "Cb and B sound the same" $ do
      sameSound (N C Flat) (N B Nat) `shouldBe` True
    it "E# and F sound the same" $ do
      sameSound (N E Sharp) (N F Nat) `shouldBe` True
    it "A# and Bb sound the same" $ do
      sameSound (N A Sharp) (N B Flat) `shouldBe` True
    it "Eb and D# sound the same" $ do
      sameSound (N E Flat) (N D Sharp) `shouldBe` True
    it "Eb and D do not sound the same" $ do
      sameSound (N E Flat) (N D Nat) `shouldBe` False

  describe "Tones and semitones" $ do
    it "C is a semitone up from B" $ do
      semitone up Sharp (N B Nat) `shouldBe` (N C Nat)
    it "B is a semitone down from C" $ do
      semitone down Sharp (N C Nat) `shouldBe` (N B Nat)
    it "F is a semitone up from E" $ do
      semitone up Flat (N E Nat) `shouldBe` (N F Nat)
    it "Eb is a semitone down from E when expecting flats" $ do
      semitone down Flat (N E Nat) `shouldBe` (N E Flat)
    it "C# is a tone up from B when expecting sharps" $ do
      tone up Sharp (N B Nat) `shouldBe` (N C Sharp)
    it "D is a tone down from E" $ do
      tone down Sharp (N E Nat) `shouldBe` (N D Nat)
    it "Db is a tone up from B when expecting flats" $ do
      tone up Flat (N B Nat) `shouldBe` (N D Flat)
    it "Db is a tone down from Eb when expecting flats" $ do
      tone down Flat (N E Flat) `shouldBe` (N D Flat)
