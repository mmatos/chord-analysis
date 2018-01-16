import Chords
import Notes
import Test.Hspec

main = hspec $ do
  describe "Major Chord creation" $ do
    it "C major has C, E and G" $ do
      notes (majorChord (N C Nat)) `shouldBe` [N C Nat, N E Nat, N G Nat]
    it "G major has G, B and D" $ do
      notes (majorChord (N G Nat)) `shouldBe` [N G Nat, N B Nat, N D Nat]
    it "Bb major has Bb, D and F" $ do
      notes (majorChord (N B Flat)) `shouldBe` [N B Flat, N D Nat, N F Nat]
  
  describe "Minor Chord creation" $ do
    it "C minor has C, Eb and G" $ do
      notes (minorChord (N C Nat)) `shouldBe` [N C Nat, N E Flat, N G Nat]
    it "G minor has G, Bb and D" $ do
      notes (minorChord (N G Nat)) `shouldBe` [N G Nat, N B Flat, N D Nat]
    it "Bb minor has Bb, Db and F" $ do
      notes (minorChord (N B Flat)) `shouldBe` [N B Flat, N D Flat, N F Nat]
  
  describe "Diminished Chord creation" $ do
    it "C dim has C, Eb and Gb" $ do
      notes (dimChord (N C Nat)) `shouldBe` [N C Nat, N E Flat, N G Flat]
    it "G dim has G, Bb and Db" $ do
      notes (dimChord (N G Nat)) `shouldBe` [N G Nat, N B Flat, N D Flat]
    it "Bb dim has Bb, Db and E" $ do
      notes (dimChord (N B Flat)) `shouldBe` [N B Flat, N D Flat, N E Nat]
  
  describe "Augmented Chord creation" $ do
    it "C aug has C, E and G#" $ do
      notes (augChord (N C Nat)) `shouldBe` [N C Nat, N E Nat, N G Sharp]
    it "G aug has G, B and D#" $ do
      notes (augChord (N G Nat)) `shouldBe` [N G Nat, N B Nat, N D Sharp]
    it "Bb aug has Bb, D and F#" $ do
      notes (augChord (N B Flat)) `shouldBe` [N B Flat, N D Nat, N F Sharp]
  
  describe "Sus Chord creation" $ do
    it "C sus2 has C, E and G" $ do
      notes (susChord 2 (N C Nat)) `shouldBe` [N C Nat, N D Nat, N G Nat]
    it "Bb sus2 has Bb, C and F" $ do
      notes (susChord 2 (N B Flat)) `shouldBe` [N B Flat, N C Nat, N F Nat]
    it "C sus4 has C, F and G" $ do
      notes (susChord 4 (N C Nat)) `shouldBe` [N C Nat, N F Nat, N G Nat]
    it "Bb sus4 has Bb, Eb and F" $ do
      notes (susChord 4 (N B Flat)) `shouldBe` [N B Flat, N E Flat, N F Nat]

  describe "Chord with minor seventh" $ do
    it "C7 has C, E, G and Bb" $ do
      notes (alterChord seventh (majorChord (N C Nat))) `shouldBe` [N C Nat, N E Nat, N G Nat, N B Flat]
    it "C-7 has C, Eb and G and Bb" $ do
      notes (alterChord seventh (minorChord (N C Nat))) `shouldBe` [N C Nat, N E Flat, N G Nat, N B Flat]

  describe "Chord with major seventh" $ do
    it "Cmaj7 has C, E, G and B" $ do
      notes (alterChord majorSeventh (majorChord (N C Nat))) `shouldBe` [N C Nat, N E Nat, N G Nat, N B Nat]
    it "C-maj7 has C, Eb and G and B" $ do
      notes (alterChord majorSeventh (minorChord (N C Nat))) `shouldBe` [N C Nat, N E Flat, N G Nat, N B Nat]

  describe "Chord with sixth" $ do
    it "C6 has C, E, G and A" $ do
      notes (alterChord sixth (majorChord (N C Nat))) `shouldBe` [N C Nat, N E Nat, N G Nat, N A Nat]
    it "C-6 has C, Eb and G and A" $ do
      notes (alterChord sixth (minorChord (N C Nat))) `shouldBe` [N C Nat, N E Flat, N G Nat, N A Nat]

  describe "Chord with ninth" $ do
    it "C-(9) should have C, Eb, G and D" $ do
      notes (alterChord (overtone 9) (minorChord (N C Nat))) `shouldBe` [N C Nat, N E Flat, N G Nat, N D Nat]
    it "C-(#9) should have C, Eb, G and D#" $ do
      notes (alterChord (augOvertone 9) (minorChord (N C Nat))) `shouldBe` [N C Nat, N E Flat, N G Nat, N D Sharp]

  describe "Chord with eleventh" $ do
    it "C-(11) should have C, Eb, G and F" $ do
      notes (alterChord (overtone 11) (minorChord (N C Nat))) `shouldBe` [N C Nat, N E Flat, N G Nat, N F Nat]
    it "C-(#11) should have C, Eb, G and F#" $ do
      notes (alterChord (augOvertone 11) (minorChord (N C Nat))) `shouldBe` [N C Nat, N E Flat, N G Nat, N F Sharp]

  describe "Chord printing" $ do
    it "Print major chord" $ do
      show (majorChord (N B Flat)) `shouldBe` "Bb"
    it "Print minor chord" $ do
      show (minorChord (N B Flat)) `shouldBe` "Bb-"
    it "Print diminished chord" $ do
      show (dimChord (N B Flat)) `shouldBe` "Bb°"
    it "Print augmented chord" $ do
      show (augChord (N B Flat)) `shouldBe` "Bb+"
    it "Print sus2 chord" $ do
      show (susChord 2 (N B Flat)) `shouldBe` "Bbsus2"
    it "Print sus4 chord" $ do
      show (susChord 4 (N B Flat)) `shouldBe` "Bbsus4"
    it "Print chord with 6th" $ do
      show (alterChord sixth (minorChord (N B Flat))) `shouldBe` "Bb-6"
    it "Print chord with 7th" $ do
      show (alterChord seventh (minorChord (N B Flat))) `shouldBe` "Bb-7"
    it "Print chord with major 7th" $ do
      show (alterChord majorSeventh (minorChord (N B Flat))) `shouldBe` "Bb-maj7"
    it "Print chord with 9th" $ do
      show (alterChord (overtone 9) (minorChord (N B Flat))) `shouldBe` "Bb-(9)"
    it "Print chord with augmented 9th" $ do
      show (alterChord (augOvertone 9) (minorChord (N B Flat))) `shouldBe` "Bb-(#9)"
    it "Print chord with 11th" $ do
      show (alterChord (overtone 11) (minorChord (N B Flat))) `shouldBe` "Bb-(11)"
    it "Print chord with augmented 11th" $ do
      show (alterChord (augOvertone 11) (minorChord (N B Flat))) `shouldBe` "Bb-(#11)"