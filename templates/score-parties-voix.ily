\score {
  <<
    \new Staff \with { \tinyStaff \haraKiriFirst } \withTinyLyrics <<
      \global \keepWithTag #'basse \includeNotes "voix"
    >> \keepWithTag #'basse \includeLyrics "paroles"
    \new StaffGroup \with { \haraKiriFirst } <<
      \new Staff <<
        \keepWithTag #(*tag-global*) \global
        \keepWithTag #'haute-contre \includeNotes #(*note-filename*)
        \clef #(*clef*)
        $(or (*score-extra-music*) (make-music 'Music))
      >>
      \new Staff <<
        \keepWithTag #(*tag-global*) \global
        \keepWithTag #'taille \includeNotes #(*note-filename*)
        \clef #(*clef*)
      >>
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
