# chord-analysis

Haskell program to work with scales, chords and progressions.

Main feature:

Be able to analyze which are the possible functions of a chord in the context of a scale (in either of the 7 ancient modes).

Example:

*ChordAnalysis> analyzeChord (minorChord (N C Sharp)) (Modal Dorian (N D Nat))
["II/VI","VII Lydian"]