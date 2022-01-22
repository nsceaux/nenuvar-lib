%%% Foot notes
%%%
%%% options: print-footnotes
%%%
\paper {
  footnote-auto-numbering = ##t
  footnote-numbering-function =
  #(lambda (num)
     (markup #:small #:box (number->string (+ 1 num))))
  footnote-separator-markup = \markup\override #'(span-factor . 1/4) \draw-hline
  footnote-padding = 2\mm
  footnote-footer-padding = 1\mm
}

\layout {
  \context {
    \Score
    \name Score
    %% By default, no annotation line for footnotes
    \override FootnoteItem #'annotation-line = ##f
  }
}

#(define (make-footnote-here-music offset note)
   (make-music 'FootnoteEvent
               'X-offset (car offset)
               'Y-offset (cdr offset)
               'automatically-numbered #t
               'text (make-null-markup)
               'footnote-text note))

footnoteHere =
#(define-music-function (parser this-location offset note)
     (number-pair? markup?)
   "If `print-footnotes' option is true, then print adds a footnote at
that point."
   (if (eqv? #t (ly:get-option 'print-footnotes))
       #{ <>-\tweak footnote-music #(make-footnote-here-music offset note)
           ^\markup\transparent\box "1" #}
       (make-music 'Music 'void #t)))

footnoteHereNoSpace =
#(define-music-function (parser this-location offset note)
     (number-pair? markup?)
   (if (eqv? #t (ly:get-option 'print-footnotes))
       #{ <>-\tweak footnote-music #(make-footnote-here-music offset note)
          ^\markup\null #}
       (make-music 'Music 'void #t)))
