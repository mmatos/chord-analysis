module ChordAnalysis where
import Notes
import Scales
import Chords

----------------------
-- Chord progressions
----------------------

modalChordProgression Ionian = [majorChord, minorChord, minorChord, majorChord, majorChord, minorChord, dimChord]
modalChordProgression mode = shiftWith modalChordProgression mode

chordProgression (Modal mode startNote) = map (\(i, chordType) -> (chordType.scaleInterval i.Modal mode) startNote) (zip [1..7] (modalChordProgression mode))

-------------------
-- Chord Functions
-------------------

nthChord n scale = (chordProgression scale) !! (n-1)

applied nth ofN scale =
  let newScale =  scale { startNote = tonic (nthChord ofN scale) }
  in nthChord nth newScale

borrow nth otherMode scale =
  let newScale = scale { mode = otherMode }
  in nthChord nth newScale
