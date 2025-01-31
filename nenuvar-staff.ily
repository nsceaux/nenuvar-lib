#(use-modules (ice-9 format))
#(define-public gen-unique-context
  ;; Generate a uniqueSchemeContextXX symbol, that may be (hopefully) unique.
  (let ((var-idx -1))
    (lambda ()
      (set! var-idx (1+ var-idx))
      (string->symbol
       (format #f "uniqueSchemeContext~a"
               (list->string (map (lambda (chr)
                                    (integer->char (+ (char->integer #\a)
                                                      (- (char->integer chr)
                                                         (char->integer #\0)))))
                                  (string->list (number->string var-idx)))))))))

haraKiri = \with {
  \override VerticalAxisGroup.remove-empty = ##t
  \override VerticalAxisGroup.remove-first = ##f
}

haraKiriFirst = \with {
  \override VerticalAxisGroup.remove-empty = ##t
  \override VerticalAxisGroup.remove-first = ##t
}

%%%
%%% On-demand hara-kiri
%%%
startHaraKiri = \set Staff.keepAliveInterfaces = #'()
stopHaraKiri = \unset Staff.keepAliveInterfaces

noHaraKiri = \set Staff.keepAliveInterfaces =
#'(multi-measure-interface
   rhythmic-grob-interface
   lyric-interface
   percent-repeat-item-interface
   percent-repeat-interface
   stanza-number-interface)
revertNoHaraKiri = \unset Staff.keepAliveInterfaces

%% The following hack make regular rests hara-kiri-able
#(let* ((rest-def (assoc 'Rest all-grob-descriptions))
        (meta-def (assoc 'meta (cdr rest-def)))
        (interfaces-def (assoc 'interfaces (cdr meta-def)))
        (interfaces (filter (lambda (interface)
                              (not (eqv? interface 'rhythmic-grob-interface)))
                            (cdr interfaces-def))))
  (set-cdr! interfaces-def interfaces))

tinyStaff = \with {
  \override StaffSymbol.staff-space = #(magstep -2)
  fontSize = #-2
}

smallStaff = \with {
  \override StaffSymbol.staff-space = #(magstep -1)
  fontSize = #-1
}

withTinyLyrics =
#(define-music-function (parser location music lyrics) (ly:music? ly:music?)
   (let ((name (symbol->string (gen-unique-context))))
     #{  << \context Voice = $name \with { autoBeaming = ##f } $music
            \new Lyrics \with { fontSize = -2 } \lyricsto #name { #lyrics }
            >> #}))


withLyrics =
#(define-music-function (parser location music lyrics) (ly:music? ly:music?)
   (let ((name (symbol->string (gen-unique-context))))
     #{  << \context Voice = $name \with { autoBeaming = ##f } $music
            \new Lyrics \lyricsto #name { #lyrics }
            >> #}))

withTinyLyricsB =
#(define-music-function (parser location music lyrics1 lyrics2) (ly:music? ly:music? ly:music?)
   (let ((name (symbol->string (gen-unique-context))))
     #{  << \context Voice = $name \with { autoBeaming = ##f } $music
            \new Lyrics \with { fontSize = -2 } \lyricsto #name { #lyrics1 }
            \new Lyrics \with { fontSize = -2 } \lyricsto #name { #lyrics2 }
            >> #}))

              
withLyricsB =
#(define-music-function (parser location music lyrics1 lyrics2) (ly:music? ly:music? ly:music?)
   (let ((name (symbol->string (gen-unique-context))))
     #{  << \context Voice = $name \with { autoBeaming = ##f } $music
            \new Lyrics \lyricsto #name { #lyrics1 }
            \new Lyrics \lyricsto #name { #lyrics2 }
            >> #}))

withLyricsC =
#(define-music-function (parser location music lyrics1 lyrics2 lyrics3)
     (ly:music? ly:music? ly:music? ly:music?)
   (let ((name (symbol->string (gen-unique-context))))
     #{  << \context Voice = $name \with { autoBeaming = ##f } $music
            \new Lyrics \lyricsto #name { #lyrics1 }
            \new Lyrics \lyricsto #name { #lyrics2 }
            \new Lyrics \lyricsto #name { #lyrics3 }
            >> #}))

withLyricsD =
#(define-music-function (parser location music lyrics1 lyrics2 lyrics3 lyrics4)
     (ly:music? ly:music? ly:music? ly:music? ly:music?)
   (let ((name (symbol->string (gen-unique-context))))
     #{  << \context Voice = $name \with { autoBeaming = ##f } $music
            \new Lyrics \lyricsto #name { #lyrics1 }
            \new Lyrics \lyricsto #name { #lyrics2 }
            \new Lyrics \lyricsto #name { #lyrics3 }
            \new Lyrics \lyricsto #name { #lyrics4 }
            >> #}))

withLyricsE =
#(define-music-function (parser location music lyrics1 lyrics2 lyrics3 lyrics4 lyrics5)
     (ly:music? ly:music? ly:music? ly:music? ly:music? ly:music?)
   (let ((name (symbol->string (gen-unique-context))))
     #{  << \context Voice = $name \with { autoBeaming = ##f } $music
            \new Lyrics \lyricsto #name { #lyrics1 }
            \new Lyrics \lyricsto #name { #lyrics2 }
            \new Lyrics \lyricsto #name { #lyrics3 }
            \new Lyrics \lyricsto #name { #lyrics4 }
            \new Lyrics \lyricsto #name { #lyrics5 }
            >> #}))

withRecit =
#(define-music-function (parser location music lyrics) (ly:music? ly:music?)
   (let ((name (symbol->string (gen-unique-context))))
     #{  << \context Voice = $name \with { autoBeaming = ##f } <<
            \set Staff . explicitClefVisibility = #end-of-line-invisible
            \override Staff . Clef #'full-size-change = ##t
            \override Score.BreakAlignment #'break-align-orders =
            ##(; end-of-line:
               (instrument-name left-edge ambitus breathing-sign
                clef key-cancellation key-signature
                time-signature custos staff-bar)
               ; unbroken
               (instrument-name left-edge ambitus breathing-sign
                staff-bar clef key-cancellation key-signature
                staff time-signature custos)
               ; begin of line
               (instrument-name left-edge ambitus breathing-sign
                clef key-cancellation key-signature staff-bar
                time-signature custos))
            $music >>
            \new Lyrics \lyricsto #name { #lyrics }
          >> #}))
