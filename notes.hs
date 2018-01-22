module Notes where
import Data.List
import Data.Maybe

data BasicNote = C | D | E | F | G | A | B deriving (Enum, Show, Eq)
data NoteAlteration = Flat | Nat | Sharp deriving Eq
data Note = N {basicNote::BasicNote, alt::NoteAlteration} deriving Eq

class WithNotes a where
  notes :: a -> [Note]


allNotes = [N basicNote alt | basicNote <- [C ..], alt <- [Nat, Flat, Sharp]]

noteToNumber (N C Nat) = 1
noteToNumber (N B Nat) = 0
noteToNumber (N F Nat) = noteToNumber (N E Sharp)
noteToNumber (N B Flat) = noteToNumber (N A Sharp)
noteToNumber (N basicNote Nat) = noteToNumber (N (pred basicNote) Nat) + 2
noteToNumber (N basicNote Flat) = noteToNumber (N basicNote Nat) - 1
noteToNumber (N basicNote Sharp) = noteToNumber (N basicNote Nat) + 1

-- Allow naturals to prevail when possible
noteFromNumber preferred number = fromMaybe (fromJust (findAlt preferred)) (findAlt Nat)
  where
    possibleNotes = filter ((== (number `mod` 12)).noteToNumber) allNotes
    findAlt needed = find ((== needed).alt) possibleNotes

sameSound note1 note2 = noteToNumber note1 == noteToNumber note2

semitone upOrDown preferredAlteration = noteFromNumber preferredAlteration.upOrDown.noteToNumber

tone upOrDown preferredAlteration = semi.semi
  where semi = semitone upOrDown preferredAlteration

up = (+1)
down = flip (-) 1

---- Pritty print

instance Show NoteAlteration where
  show Flat = "b"
  show Nat = ""
  show Sharp = "#"

instance Show Note where
  show (N basicNote alt) = show basicNote ++ show alt

instance Read Note where
  readsPrec _ string = map (\note -> (note , "")) (filter ((== string).show) allNotes)

readNote = (\s -> read s :: Note)
readNotes = map readNote