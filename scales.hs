module Scales where
import Notes

data Mode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian deriving (Eq, Show, Enum, Bounded)
data Scale = Modal Mode Note

instance WithNotes Scale where
  notes (Modal mode startNote) = (normalizeScale. map (lookAlike startNote). foldNotesFrom startNote . scale) mode
    where 
      normalizeScale scaleNotes | (alt startNote == Nat) && shouldHaveFlats (Modal mode startNote) = map flattenize scaleNotes
                                | otherwise = scaleNotes

scale Ionian = tone : tone : semitone : tone : tone : tone : semitone : []
scale mode = shiftWith scale mode

isMajorMode = flip elem [Ionian, Lydian, Mixolydian]
isMinorMode = not.isMajorMode
naturalNote mode = N ([C .. B] !! (fromEnum mode)) Nat
shouldHaveFlats (Modal mode startNote) = (elem startNote . drop 6 . circleOfFifths . naturalNote) mode

interval :: Int -> Scale -> Note
interval n scale = notes scale !! mod (n-1) 7



----  AUX
shiftWith f = shiftFirst.f.prev
shiftFirst (x:xs) = xs ++ [x]
