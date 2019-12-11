\version "2.15.20"
\include "../nenuvar-baroque.ily"

{
  c'' \cesure c'' \cesureCenter c'' \cesureDown c'' \cesureInstr
}

{
  c'\tr c'''\tr
  c'\arcTrill c'''\arcTrill
  c'\arcDot c'''\arcDot
  c'\dotDot c'''\dotDot
  c'\dotPrall c'''\dotPrall
  c'\dotDoublePrallDoublePrall c'''\dotDoublePrallDoublePrall
  c'\doublePrall c''\doublePrall
}

{
  \slurPrall c'''( b'')
}

{
  \time 3/2
  \whiteNoteHeadsOn
  c''2 c''8.*2 c''16*2 c''2
}

%% Black notation
testPattern = { c'1 c'\breve aes'1 ais'\breve.*2/3 \break }
\score {
  {
    \time 6/1
    \override NoteHead #'style = #'baroque
    \testPattern \blackNotation\testPattern
    \override NoteHead #'style = #'petrucci
    \testPattern \blackNotation\testPattern
  }
  \layout {
    indent = 0 ragged-right = ##t
    \context { \Staff \remove "Time_signature_engraver" }
  }
}
