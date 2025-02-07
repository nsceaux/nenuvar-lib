\score {
  \new StaffGroup <<
    \new Staff <<
      \keepWithTag #(*tag-global*) \global
      \keepWithTag #'haute-contre \includeNotes #(*note-filename*)
      \clef #(*clef*)
      $(or (*score-extra-music*) (make-music 'Music))
    >>
    \new Staff \with { \haraKiriFirst } <<
      \keepWithTag #(*tag-global*) \global
      \keepWithTag #'taille \includeNotes #(*note-filename*)
      \clef #(*clef*)
    >>
  >>
  \layout {
    system-count = #(*system-count*)
    indent = #(if (*instrument-name*)
                  largeindent
                  (or (*score-indent*) smallindent))
    ragged-last = #(*score-ragged*)
  }
}
