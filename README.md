# haskell_diatonic_scales
Showing Diatonic Chords Tool

# Description
This is very simple haskell program which shows diatonic chords from given key.

# Requirements
The following program/library are required.
* Haskell (hugs are tested but ghc also works)
* Parsec module

# How to use:

1. run chord.hs
1. 'cc <key>' to select key
1. 'dia' shows diatonic chords
1. When finished, 'exit'.

Please see following example for the detail.

# Example
```
  $ hugs chord.hs
   (snip)
  Chord> main:
  C>cc Cm
  Cm>dia
  Cm7, Dm7(b5), D#M7, Fm7, Gm7, G#M7, A#7
  Cm>cc D#m
  D#m>dia
  D#m7, Fm7(b5), F#M7, G#m7, A#m7, BM7, C#7
  D#m>cc F#
  F#>dia
  F#M7, G#m7, A#m7, BM7, C#7, D#m7, Fm7(b5)
  F#>exit
```
