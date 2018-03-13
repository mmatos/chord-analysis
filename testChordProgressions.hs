import ChordAnalysis
import Chords
import Notes
import Scales
import Test.Hspec

main = hspec $ do
  describe "Chord progressions in a scale" $ do
    it "Chord progression for C Ionian" $ do
      chordProgression (Modal Ionian (N C Nat)) `shouldBe` 
        [majorChord (N C Nat), minorChord (N D Nat), minorChord (N E Nat),
         majorChord (N F Nat), majorChord (N G Nat), minorChord (N A Nat), dimChord (N B Nat)]
    it "Chord progression for F Ionian" $ do
      chordProgression (Modal Ionian (N F Nat)) `shouldBe` 
        [majorChord (N F Nat), minorChord (N G Nat), minorChord (N A Nat),
         majorChord (N B Flat), majorChord (N C Nat), minorChord (N D Nat), dimChord (N E Nat)]
    it "Chord progression for D Dorian" $ do
      chordProgression (Modal Dorian (N D Nat)) `shouldBe` 
        [minorChord (N D Nat), minorChord (N E Nat), majorChord (N F Nat),
         majorChord (N G Nat), minorChord (N A Nat), dimChord (N B Nat), majorChord (N C Nat)]
    it "Chord progression for F Dorian" $ do
      chordProgression (Modal Dorian (N F Nat)) `shouldBe` 
        [minorChord (N F Nat), minorChord (N G Nat), majorChord (N A Flat),
         majorChord (N B Flat), minorChord (N C Nat), dimChord (N D Nat), majorChord (N E Flat)]
    it "Chord progression for A Aeolian" $ do
      chordProgression (Modal Aeolian (N A Nat)) `shouldBe` 
        [minorChord (N A Nat), dimChord (N B Nat), majorChord (N C Nat), 
         minorChord (N D Nat), minorChord (N E Nat), majorChord (N F Nat), majorChord (N G Nat)]
    it "Chord progression for F Aeolian" $ do
      chordProgression (Modal Aeolian (N F Nat)) `shouldBe` 
        [minorChord (N F Nat), dimChord (N G Nat), majorChord (N A Flat),
         minorChord (N B Flat), minorChord (N C Nat), majorChord (N D Flat), majorChord (N E Flat)]