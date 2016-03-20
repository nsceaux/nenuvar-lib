#(define-markup-command (character-ambitus layout props old-clef new-clef ambitus)
     (string? string? ly:music?)
   (let* ((low-pitch (ly:music-property
                      (car (ly:music-property ambitus 'elements))
                      'pitch))
          (high-pitch (ly:music-property
                       (cadr (ly:music-property ambitus 'elements))
                       'pitch))
          (chord (make-music
                  'EventChord
                  'elements (list (make-music
                                   'NoteEvent
                                   'duration (ly:make-duration 2 0 1 1)
                                   'pitch low-pitch)
                                  (make-music
                                   'NoteEvent
                                   'duration (ly:make-duration 2 0 1 1)
                                   'pitch high-pitch))))
          (score #{ \markup { \null \raise#1 \score {
  \new Staff {
    \clef $old-clef s8
    \set Staff.forceClef = ##t \clef $new-clef s8
    $chord
  }
  \layout {
    \quoteLayout
    line-width = #14
    ragged-right = ##f
    \context {
      \Staff
      \override Clef #'full-size-change = ##t
      \remove "Bar_engraver"
    }
    \context {
      \Voice
      \remove "Stem_engraver"
    }
    \context {
      \Score
      \override StaffSymbol #'staff-space = #(magstep -2)
      fontSize = #-2
      \override NonMusicalPaperColumn #'line-break-permission = ##f
    }
  }
} \hspace#1 } #})
                 (score-stencil (interpret-markup layout props score)))
          (ly:make-stencil (ly:stencil-expr score-stencil)
                           (ly:stencil-extent score-stencil X)
                           '(-2.5 . 4.5))))
