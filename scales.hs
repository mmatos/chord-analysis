module Scales where
import Notes

data Mode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian deriving (Eq, Show, Enum)
data Scale = Modal {mode :: Mode, startNote :: Note}

instance WithNotes Scale where
  notes (Modal mode startNote) = (init . foldNotesFrom startNote . scale alteration) mode
    where alteration = sharpsOrFlats (Modal mode startNote)

sharpsOrFlats (Modal mode startNote)
  | (elem startNote . drop 6 . circleOfFifths . naturalNote) mode = Flat
  | otherwise = Sharp

scale preferredAlteration = map (\f -> f up preferredAlteration).scalePattern

scalePattern Ionian = tone : tone : semitone : tone : tone : tone : semitone : []
scalePattern mode = shiftWith scalePattern mode

naturalNote mode = N ([C .. B] !! (fromEnum mode)) Nat

scaleInterval :: Int -> Scale -> Note
scaleInterval n scale = notes scale !! mod (n-1) 7

chromatics preferredAlteration = iterate (semitone up preferredAlteration)

circleOfFifths = flip foldNotesFrom ((take 6 (fifthsWith Sharp)) ++ (take 6 (drop 6 (fifthsWith Flat))))
  where
    fifthsWith preferredAlteration = (repeat.perfectFifth) preferredAlteration

perfectFifth preferredAlteration = head.drop 7.chromatics preferredAlteration

----
-- Common scale creation
----

majorScale = Modal Ionian
minorScale = Modal Aeolian

----  AUX
shiftWith f = shiftFirst.f.pred
shiftFirst (x:xs) = xs ++ [x]

foldNotesFrom startNote = foldl (\notes f -> notes ++ [(f.last) notes]) [startNote]