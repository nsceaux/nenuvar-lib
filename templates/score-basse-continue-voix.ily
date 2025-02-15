\score {
  \new StaffGroupNoBar <<
    \new Staff \with { \haraKiriFirst \tinyStaff } \withTinyLyrics <<
      \keepWithTag #(*tag-global*) \global
      \keepWithTag #'basse \includeNotes "voix"
    >> \keepWithTag #'basse \includeLyrics "paroles"
    \new Staff \with { \haraKiriFirst } <<
      \keepWithTag #(*tag-global*) \global
      \keepWithTag #(*tag-notes*) \includeNotes #(*note-filename*)
      \clef #(*clef*)
      $(if (*instrument-name*)
                      (make-music 'ContextSpeccedMusic
                                  'context-type 'Staff
                                  'element (make-music 'PropertySet
                                                       'value (markup #:large (*instrument-name*))
                                                       'symbol 'instrumentName))
                      (make-music 'Music))
      $(or (*score-extra-music*) (make-music 'Music))
      \keepWithTag #(*tag-notes*) \includeFigures "chiffres"
    >>
  >>
  \layout {
    system-count = #(or (*system-count*) #f)
    indent = #(if (*instrument-name*)
                  largeindent
                  (or (*score-indent*) smallindent))
    ragged-last = #(*score-ragged*) }
}
