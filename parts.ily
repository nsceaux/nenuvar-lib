#(define-markup-command (tacet layout props num) (number?)
   (let ((score (ly:make-score
                 (make-music
                  'SequentialMusic
                  'elements (list (make-music
                                   'MultiMeasureRestMusic
                                   'duration (ly:make-duration 0 0 num))
                                  (make-music
                                   'ContextSpeccedMusic
                                   'context-type 'Timing
                                   'element (make-music
                                             'PropertySet
                                             'value ""
                                             'symbol 'whichBar)))))))
     (ly:score-add-output-def! score #{ \layout {
      indent = 0
      ragged-right = ##t
      \context {
        \Score
        skipBars = ##t
        fontSize = #-4
        \override StaffSymbol.staff-space = #(magstep -4)
      }
      \context {
        \Staff
        \remove "Time_signature_engraver"
        \remove "Clef_engraver"
        \override StaffSymbol.line-count = #1
        \override StaffSymbol.transparent = ##t
        \override MultiMeasureRest #'expand-limit = #2
      }
    } #})
     (interpret-markup
      layout props
      #{ \markup { \hspace #10 Tacet
          \with-dimensions #'(-1 . 1) #'(-1 . 2) $(make-score-markup score) } #})))
