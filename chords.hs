module Chords where
import Data.List
import Notes
import Scales

data Chord = Ch {note::Note, alterations::[ChordAlteration]}
data ChordAlteration = ChAlt {nameAlt::String, chordAlterations::([(Int,Note)] -> [(Int, Note)])}

instance WithNotes Chord where
  notes chord = (map snd.sortOn fst.foldl (\numberedNotes chalt-> chordAlterations chalt numberedNotes) (zip [1,3,5] baseChord).alterations) chord
    where baseChord = (map ($(note chord)) [id, major 3, (\note -> perfectFifth (alt note) note)])

instance Eq Chord where
  (==) chord1 chord2 = notes chord1 == notes chord2
alterChord alteration chord = chord {alterations = alterations chord ++ [alteration]}

-------------------------
-- Common chord creation
-------------------------

majorChord note = Ch note []
minorChord note = Ch note [ChAlt "-" lowerThird]
augChord note = Ch note [ChAlt "+" (semitoneUp 5)]
dimChord note = Ch note [ChAlt "°" (lowerThird.lowerFifth)]
susChord n note | n == 2 || n == 4 = alteredChord
  where alteredChord = alterChord (ChAlt ("sus"++show n)(replace 3 (\_ -> major n))) (majorChord note)

dim7Chord = alterChord (ChAlt "7" (add 6 major)).dimChord
halfDim7Chord = alterChord flatFive.alterChord seventh.minorChord

chord7 = alterChord seventh
chordMaj7 = alterChord majorSeventh

--------------------------------
-- Additional chord alterations
-- Use with alterChord
--------------------------------

flatFive = ChAlt "(b5)" lowerFifth
sixth = ChAlt "6" (add 6 major)
seventh = ChAlt "7" (add 7 minor)
majorSeventh = ChAlt "maj7" (add 7 major)

overtone n = ChAlt ("("++show n++")") (add n major)
augOvertone n = ChAlt ("(#"++show n++")") (semitoneUp n . add n major)

----------------------------
-- Common alteration combos
----------------------------

upToOvertone n startChord = (foldl (flip alterChord) (chord7 startChord).map overtone.filter odd) [9 .. n]

-----------------------------
-- Base alteration functions
-----------------------------

major n  = interval n . Modal Ionian
minor n  = interval n . Modal Aeolian
lowerThird = replace 3 minor
lowerFifth = replace 5 (\n -> interval n.Modal Locrian)

-- Is it ok to make this always major?
semitoneUp n = replace n (\m -> semitone up Sharp . major m)

replace n intervalFunction numberedNotes = add n intervalFunction (filter ((/= n).fst) numberedNotes)

add n intervalFunction numberedNotes = (n, (intervalFunction n) firstNote) : numberedNotes
  where firstNote = (snd.head.filter ((==1).fst)) numberedNotes
  
----------------------
-- Chord progressions
----------------------

modalChordProgression Ionian = [majorChord, minorChord, minorChord, majorChord, majorChord, minorChord, dimChord]
modalChordProgression mode = shiftWith modalChordProgression mode

chordProgression (Modal mode startNote) = map (\(i, chordType) -> (chordType.interval i.Modal mode) startNote) (zip [1..7] (modalChordProgression mode))

---- Pritty print

instance Show Chord where
  show (Ch note alterations) = show note ++ (alterations >>= show)
  
instance Show ChordAlteration where
  show (ChAlt name _) = name