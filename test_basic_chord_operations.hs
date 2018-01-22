import Chords
import Notes
import Test.Hspec
import TestHelpers

main = hspec $ do
  describe "Major Chord creation" $ do
    it "C major has C, E and G" $ do
      majorChord (N C Nat) `shouldHaveNotes` ["C", "E", "G"]
    it "G major has G, B and D" $ do
      majorChord (N G Nat) `shouldHaveNotes` ["G", "B", "D"]
    it "Bb major has Bb, D and F" $ do
      majorChord (N B Flat) `shouldHaveNotes` ["Bb", "D", "F"]
  
  describe "Minor Chord creation" $ do
    it "C minor has C, Eb and G" $ do
      minorChord (N C Nat) `shouldHaveNotes` ["C", "Eb", "G"]
    it "G minor has G, Bb and D" $ do
      minorChord (N G Nat) `shouldHaveNotes` ["G", "Bb", "D"]
    it "Bb minor has Bb, Db and F" $ do
      minorChord (N B Flat) `shouldHaveNotes` ["Bb", "Db", "F"]

  describe "Augmented Chord creation" $ do
    it "C aug has C, E and G#" $ do
      augChord (N C Nat) `shouldHaveNotes` ["C", "E", "G#"]
    it "G aug has G, B and D#" $ do
      augChord (N G Nat) `shouldHaveNotes` ["G", "B", "D#"]
    it "Bb aug has Bb, D and F#" $ do
      augChord (N B Flat) `shouldHaveNotes` ["Bb", "D", "F#"]
    
  describe "Diminished Chord creation" $ do
    it "C dim has C, Eb and Gb" $ do
      dimChord (N C Nat) `shouldHaveNotes` ["C", "Eb", "Gb"]
    it "G dim has G, Bb and Db" $ do
      dimChord (N G Nat) `shouldHaveNotes` ["G", "Bb", "Db"]
    it "Bb dim has Bb, Db and E" $ do
      dimChord (N B Flat) `shouldHaveNotes` ["Bb", "Db", "E"]
  
  describe "Sus Chord creation" $ do
    it "C sus2 has C, D and G" $ do
      susChord 2 (N C Nat) `shouldHaveNotes` ["C", "D", "G"]
    it "Bb sus2 has Bb, C and F" $ do
      susChord 2 (N B Flat) `shouldHaveNotes` ["Bb", "C", "F"]
    it "C sus4 has C, F and G" $ do
      susChord 4 (N C Nat) `shouldHaveNotes` ["C", "F", "G"]
    it "Bb sus4 has Bb, Eb and F" $ do
      susChord 4 (N B Flat) `shouldHaveNotes` ["Bb", "Eb", "F"]

  describe "Chord with minor seventh" $ do
    it "C7 has C, E, G and Bb" $ do
      chord7 (majorChord (N C Nat)) `shouldHaveNotes`  ["C", "E", "G", "Bb"]
    it "C-7 has C, Eb and G and Bb" $ do
      chord7 (minorChord (N C Nat)) `shouldHaveNotes` ["C", "Eb", "G", "Bb"]
    it "C+7 has C, E and G# and Bb" $ do
      chord7 (augChord (N C Nat)) `shouldHaveNotes` ["C", "E", "G#", "Bb"]

  describe "Chord with major seventh" $ do
    it "Cmaj7 has C, E, G and B" $ do
      chordMaj7 (majorChord (N C Nat)) `shouldHaveNotes` ["C", "E", "G", "B"]
    it "C-maj7 has C, Eb and G and B" $ do
      chordMaj7 (minorChord (N C Nat)) `shouldHaveNotes` ["C", "Eb", "G", "B"]

  describe "Chord with sixth" $ do
    it "C6 has C, E, G and A" $ do
      alterChord sixth (majorChord (N C Nat)) `shouldHaveNotes` ["C", "E", "G", "A"]
    it "C-6 has C, Eb and G and A" $ do
      alterChord sixth (minorChord (N C Nat)) `shouldHaveNotes` ["C", "Eb", "G", "A"]

  describe "Half-diminished chord creation" $ do
    it "C-7(b5) has C, Eb and Gb and Bb" $ do
      halfDim7Chord (N C Nat) `shouldHaveNotes` ["C", "Eb", "Gb", "Bb"]
    it "Bb-7(b5) has Bb, Db, E and Ab" $ do
      halfDim7Chord (N B Flat) `shouldHaveNotes` ["Bb", "Db", "E", "Ab"]

  describe "Dim7 chord creation" $ do
    it "C°7 has C, Eb and Gb and A" $ do
      dim7Chord (N C Nat) `shouldHaveNotes` ["C", "Eb", "Gb", "A"]
    it "Bb°7 has Bb, Db, E and G" $ do
      dim7Chord (N B Flat) `shouldHaveNotes` ["Bb", "Db", "E", "G"]

  describe "Chord with ninth" $ do
    it "C-(9) should have C, Eb, G and D" $ do
      alterChord (overtone 9) (minorChord (N C Nat)) `shouldHaveNotes` ["C", "Eb", "G", "D"]
    it "C-(#9) should have C, Eb, G and D#" $ do
      alterChord (augOvertone 9) (minorChord (N C Nat)) `shouldHaveNotes` ["C", "Eb", "G", "D#"]

  describe "Chord with eleventh" $ do
    it "C-(11) should have C, Eb, G and F" $ do
      alterChord (overtone 11) (minorChord (N C Nat)) `shouldHaveNotes` ["C", "Eb", "G", "F"]
    it "C-(#11) should have C, Eb, G and F#" $ do
      alterChord (augOvertone 11) (minorChord (N C Nat)) `shouldHaveNotes` ["C", "Eb", "G", "F#"]

  describe "Chord up to eleventh" $ do
    it "C7(9)(11) should have C, E, G, Bb, D and F" $ do
      upToOvertone 11 (majorChord (N C Nat)) `shouldHaveNotes` ["C", "E", "G", "Bb", "D", "F"]
    it "C-7(9)(11) should have C, Eb, G, Bb, D and F" $ do
      upToOvertone 11 (minorChord (N C Nat)) `shouldHaveNotes` ["C", "Eb", "G", "Bb", "D", "F"]

  describe "Chord printing" $ do
    it "Print major chord" $ do
      majorChord (N B Flat) `shouldShowLike` "Bb"
    it "Print minor chord" $ do
      minorChord (N B Flat) `shouldShowLike` "Bb-"
    it "Print diminished chord" $ do
      dimChord (N B Flat) `shouldShowLike` "Bb°"
    it "Print augmented chord" $ do
      augChord (N B Flat) `shouldShowLike` "Bb+"
    it "Print sus2 chord" $ do
      susChord 2 (N B Flat) `shouldShowLike` "Bbsus2"
    it "Print sus4 chord" $ do
      susChord 4 (N B Flat) `shouldShowLike` "Bbsus4"
    it "Print major chord with b5" $ do
      alterChord flatFive (majorChord (N B Flat)) `shouldShowLike` "Bb(b5)"
    it "Print chord with 6th" $ do
      alterChord sixth (minorChord (N B Flat)) `shouldShowLike` "Bb-6"
    it "Print chord with 7th" $ do
      chord7 (minorChord (N B Flat)) `shouldShowLike` "Bb-7"
    it "Print chord with major 7th" $ do
      chordMaj7 (minorChord (N B Flat)) `shouldShowLike` "Bb-maj7"
    it "Print half-diminished chord" $ do
      halfDim7Chord (N B Flat) `shouldShowLike` "Bb-7(b5)"
    it "Print diminished with 7 chord" $ do
      dim7Chord (N B Flat) `shouldShowLike` "Bb°7"
    it "Print chord with 9th" $ do
      alterChord (overtone 9) (minorChord (N B Flat)) `shouldShowLike` "Bb-(9)"
    it "Print chord with augmented 9th" $ do
      alterChord (augOvertone 9) (minorChord (N B Flat)) `shouldShowLike` "Bb-(#9)"
    it "Print chord with 11th" $ do
      alterChord (overtone 11) (minorChord (N B Flat)) `shouldShowLike` "Bb-(11)"
    it "Print chord with augmented 11th" $ do
      alterChord (augOvertone 11) (minorChord (N B Flat)) `shouldShowLike` "Bb-(#11)"
    it "Print chord up to 11th overtone" $ do
      upToOvertone 11 (minorChord (N B Flat)) `shouldShowLike` "Bb-7(9)(11)"