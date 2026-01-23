\score {
  \new StaffGroup <<
    \new Staff <<
      \keepWithTag #(*tag-global*) \global
      \includeNotes "haute-contre"
      \clef #(*clef*)
      $(or (*score-extra-music*) (make-music 'Music))
    >>
    \new Staff \with { \haraKiriFirst } <<
      \keepWithTag #(*tag-global*) \global
      \includeNotes "taille"
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
