\version "2.19.37"
\include "../nenuvar-clef.ily"

#(ly:set-option 'use-ancient-clef #t)
\markup\bold "Ancient clefs"
\markup "Expected:"
{ \clef "french" c'1
  \clef "petrucci-g" c'1
  \clef "petrucci-c1" c'1
  \clef "petrucci-c2" c'1
  \clef "petrucci-c3" c'1
  \clef "petrucci-c3" c'1
  \clef "petrucci-c4" c'1
  \clef "petrucci-f" c'1
  \clef "petrucci-c5" c'1 }
\markup "Obtained:"
{ \clef "dessus" c'1
  \clef "petrucci-g/treble" c'1
  \clef "petrucci-c1/treble" c'1
  \clef "petrucci-c2/treble" c'1
  \clef "petrucci-c3/treble" c'1
  \clef "petrucci-c3/G_8" c'1
  \clef "petrucci-c4/G_8" c'1
  \clef "petrucci-f/bass" c'1
  \clef "petrucci-c5/bass" c'1 }

#(ly:set-option 'use-ancient-clef #f)
\markup\bold "Modern clefs"
\markup "Expected:"
{ \clef "treble" c'1
  \clef "treble" c'1
  \clef "treble" c'1
  \clef "treble" c'1
  \clef "treble" c'1
  \clef "G_8" c'1
  \clef "G_8" c'1
  \clef "bass" c'1
  \clef "bass" c'1 }
\markup "Obtained:"
{ \clef "dessus" c'1
  \clef "petrucci-g/treble" c'1
  \clef "petrucci-c1/treble" c'1
  \clef "petrucci-c2/treble" c'1
  \clef "petrucci-c3/treble" c'1
  \clef "petrucci-c3/G_8" c'1
  \clef "petrucci-c4/G_8" c'1
  \clef "petrucci-f/bass" c'1
  \clef "petrucci-c5/bass" c'1 }

#(ly:set-option 'show-ancient-clef #t)
\markup\bold "Ancient and modern clefs (without parentheses)"
{ \clef "dessus" c'1
  \clef "petrucci-g/treble" c'1
  \clef "petrucci-c1/treble" c'1
  \clef "petrucci-c2/treble" c'1
  \clef "petrucci-c3/treble" c'1
  \clef "petrucci-c3/G_8" c'1
  \clef "petrucci-c4/G_8" c'1
  \clef "petrucci-f/bass" c'1
  \clef "petrucci-c5/bass" c'1 }
