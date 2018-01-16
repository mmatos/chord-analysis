module Chords where
import Data.List
import Notes
import Scales

data Chord = Ch {note::Note, alterations::[ChordAlteration]}
data ChordAlteration = ChAlt {nameAlt::String, chordAlterations::([(Int,Note)] -> [(Int, Note)])}

instance WithNotes Chord where
  notes chord = (map snd.sortOn fst.foldl (\numberedNotes chalt-> chordAlterations chalt numberedNotes) (zip [1,3,5] baseChord).alterations) chord
    where baseChord = (map ($(note chord)) [id, major 3, perfectFifth])

major n  = interval n . Modal Ionian
minor n  = interval n . Modal Aeolian

majorChord note = Ch note []
minorChord note = Ch note [ChAlt "-" lowerThird]
dimChord note = Ch note [ChAlt "°" (lowerThird.lowerFifth)]
augChord note = Ch note [ChAlt "+" (semitoneUp 5)]

replace n intervalFunction numberedNotes = add n intervalFunction (filter ((/= n).fst) numberedNotes)

add n intervalFunction numberedNotes = (n, (intervalFunction n) firstNote) : numberedNotes
  where firstNote = (snd.head.filter ((==1).fst)) numberedNotes
  
alterChord alteration chord = chord {alterations = alterations chord ++ [alteration]}

sixth = ChAlt "6" (add 6 major)
seventh = ChAlt "7" (add 7 minor)
majorSeventh = ChAlt "maj7" (add 7 major)

ninth = ChAlt "9" (add 9 major)

lowerThird = replace 3 minor
lowerFifth = replace 5 (\n -> interval n.Modal Locrian)
semitoneUp n = replace n (\m -> semitone.interval m.Modal Ionian)

modalChordProgression Ionian = [majorChord, minorChord, minorChord, majorChord, majorChord, minorChord, dimChord]
modalChordProgression mode = shiftWith modalChordProgression mode

chordProgression (Modal mode startNote) = map (\(i, chordType) -> (chordType.interval i.Modal mode) startNote) (zip [1..7] (modalChordProgression mode))

---- Pritty print

instance Show Chord where
  show (Ch note alterations) = show note ++ (alterations >>= show)
  
instance Show ChordAlteration where
  show (ChAlt name _) = name