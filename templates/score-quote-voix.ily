\score {
  \new Staff \with { \haraKiriFirst } <<
    \keepWithTag #(*tag-global*) \global
    \keepWithTag #(*tag-notes*) \includeNotes #(*note-filename*)
    \clef #(*clef*)
    \new CueVoice = "voix" \with {
      \remove "Rest_engraver"
      \remove "Multi_measure_rest_engraver"
      autoBeaming = ##f
    } { \keepWithTag #'basse \clefsToCues\includeNotes "voix" }
    \new Lyrics \with { fontSize = -2 } \lyricsto "voix" {
      \keepWithTag #'basse \includeLyrics "paroles"
    }
    $(if (*instrument-name*)
         (make-music 'ContextSpeccedMusic
           'context-type 'Staff
           'element (make-music 'PropertySet
                      'value (markup #:large (*instrument-name*))
                      'symbol 'instrumentName))
         (make-music 'Music))
    $(or (*score-extra-music*) (make-music 'Music))
  >>
  \layout {
    system-count = #(or (*system-count*) #f)
    indent = #(if (*instrument-name*)
                  largeindent
                  (or (*score-indent*) smallindent))
    ragged-last = #(*score-ragged*)
  }
}
