module Notes where

data BasicNote = C | D | E | F | G | A | B deriving (Enum, Ord, Show, Eq, Bounded)
data NoteAlteration = Flat | Nat | Sharp deriving Eq
data Note = N {basicNote::BasicNote, alt::NoteAlteration} deriving Eq
  
class WithNotes a where
  notes :: a -> [Note]

semitone (N E Nat) = (N F Nat)
semitone (N B Nat) = (N C Nat)
semitone (N basicNote Nat) = (N basicNote Sharp)
semitone (N basicNote Sharp) = (N (next basicNote) Nat)
semitone (N basicNote Flat) = (N  basicNote Nat)

tone = semitone.semitone

circleOfFifths = flip foldNotesFrom ((take 12.repeat) perfectFifth)
perfectFifth = head.drop 7.chromatics

chromatics = iterate semitone

foldNotesFrom startNote = foldl (\notes f -> notes ++ [(f.last) notes]) [startNote]

flattenize = lookAlike (N A Flat)

lookAlike (N _ Flat) (N otherBasicNote Sharp) = N (next otherBasicNote) Flat
lookAlike (N _ Sharp) (N otherBasicNote Flat) = N (prev otherBasicNote) Sharp
lookAlike _ note = note

---- Pritty print

instance Show NoteAlteration where
  show Flat = "b"
  show Nat = ""
  show Sharp = "#"

instance Show Note where
  show (N basicNote alt) = show basicNote ++ show alt

----  AUX

next :: (Enum a, Bounded a) => a -> a
next = turn 1

prev :: (Enum a, Bounded a) => a -> a
prev = turn (-1)

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
    where
      add mod x y = (x + y + mod) `rem` mod