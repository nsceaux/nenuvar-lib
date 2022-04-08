ambitusLayout = \layout {
  indent = 0
  line-width = #12
  ragged-right = ##f
  \context {
    \Staff
    \override Clef #'full-size-change = ##t
    \remove "Bar_engraver"
    \remove "Time_signature_engraver"
    \override VerticalAxisGroup.default-staff-staff-spacing.basic-distance = #0
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

#(define (make-ambitus-staff clef ambitus)
   (let* ((chord (make-music
                  'EventChord
                  'elements (list
                             (make-music
                              'NoteEvent
                              'duration (ly:make-duration 2 0 1 1)
                              'pitch (ly:music-property
                                      (car (ly:music-property ambitus 'elements))
                                      'pitch))
                             (make-music
                              'NoteEvent
                              'duration (ly:make-duration 2 0 1 1)
                              'pitch (ly:music-property
                                      (cadr (ly:music-property ambitus 'elements))
                                      'pitch))))))
     #{\new Staff { \clef $clef $chord }#}))
%%

#(define-markup-command (ambitus layout props clef ambitus)
     (string? ly:music?)
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
  $(make-ambitus-staff clef ambitus)
  \layout { \ambitusLayout }
} \hspace#1 } #})
                 (score-stencil (interpret-markup layout props score)))
          (ly:make-stencil (ly:stencil-expr score-stencil)
                           (ly:stencil-extent score-stencil X)
                           '(-2.5 . 4.5))))

#(define-markup-command (ambiti layout props clefs ambiti)
     (list? ly:music?)
   (let* ((choir-staff
           (make-music
            'SimultaneousMusic
            'elements
            (map (lambda (clef ambitus)
                   (make-ambitus-staff clef ambitus))
                 clefs
                 (ly:music-property ambiti 'elements))))
          (score #{ \markup\vcenter\score {
    $choir-staff
  \layout { \ambitusLayout }
  }#}))
          (interpret-markup layout props score)))
%%

#(define-markup-command (character-ambitus layout props name clef ambitus)
     (markup? string? ly:music?)
   "Example: \\character-ambitus \\smallCaps Atys \"vhaute-contre\" { mi la' }"
   (interpret-markup layout props #{
\markup\column {
  \fill-line {
    \override #`(line-width . ,(- (chain-assoc-get 'line-width props)
                                 14))
    \vcenter $name \ambitus $clef $ambitus
  }
  \vspace#1
}#}))

%%
#(define-markup-command (choir-ambitus layout props name clefs ambiti)
(markup? list? ly:music?)
   (interpret-markup layout props #{
\markup\column {
   \fill-line {
     \override #`(line-width . ,(- (chain-assoc-get 'line-width props)
                                   14))
     \vcenter $name \ambiti $clefs $ambiti
  }
  \vspace#1
}#}))
%%
