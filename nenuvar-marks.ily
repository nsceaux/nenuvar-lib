%%%
%%% Character marks
%%%
#(define-markup-command (characteri paper props name)
     (markup?)
  (interpret-markup
   paper props
   #{ \markup\larger\smallCaps $name #}))

#(define-markup-command (character paper props name)
     (markup?)
  (interpret-markup
   paper props
   #{ \markup\translate #'(-1 . 1) \characteri $name #}))

#(define-markup-command (character-text paper props name text)
     (markup? markup?)
  (interpret-markup
   paper props
   #{ \markup\translate #'(-1 . 1) \line {
  \characteri $name \italic $text } #}))

#(define-markup-command (character-text-col paper props name text)
     (markup? markup?)
  (interpret-markup
   paper props
   #{ \markup\translate #'(-4 . 0) \column {
  \characteri $name \translate #'(4 . 0) $text } #}))

#(define-public (make-character-mark clefs name)
   (if (string=? clefs "")
       #{ <>^\markup\character $name #}
       #{ << { \set Staff.forceClef = ##t \clef #clefs
               \once\override Staff.Clef.full-size-change = ##t }
             <>^\markup\character $name >> #}))

#(define-public (make-character-mark-text clefs name text)
   (if (string=? clefs "")
       #{ <>^\markup\character-text $name $text #}
       #{ << { \set Staff.forceClef = ##t \clef #clefs
               \once\override Staff.Clef.full-size-change = ##t }
             <>^\markup\character-text $name $text >> #}))

#(define-public (make-character-mark-text-col clefs name text)
   (if (string=? clefs "")
       #{ <>^\markup\character-text-col $name $text #}
       #{ << { \set Staff.forceClef = ##t \clef #clefs
               \once\override Staff.Clef.full-size-change = ##t }
             <> ^\markup\character-text-col $name $text >> #}))

%%%
%%% Text mark engraver and commands
%%%

%% this is taken and adapted from scm/lily/scheme-engravers.scm
#(define (Text_mark_engraver context)
   (let ((evs '())
         (grobs '()))
     (make-engraver
      (listeners
       ((text-mark-event engraver event)
        (unless (member event evs)
           (set! evs (cons event evs)))))
      ((process-music engraver)
       (let ((i 0))
         (for-each
          (lambda (ev)
            (let* ((grob (ly:engraver-make-grob engraver 'TextMark ev))
                   ;; FIXME: this is not the only place where we sneakily modify
                   ;; values of outside-staff-priority to enforce a deterministic
                   ;; order.  Improve.
                   (osp (ly:grob-property grob 'outside-staff-priority #f)))
              (when osp ; if unset, leave it unset
                    (ly:grob-set-property! grob 'outside-staff-priority (+ osp i))
                    (set! i (1+ i)))
              (set! grobs (cons grob grobs))))
          ;; The default order has the first mark heard closest to the staff.
          (reverse evs))))
      ((stop-translation-timestep engraver)
       (let ((staves (ly:context-property context 'stavesFound)))
         (for-each
          (lambda (grob)
            (ly:grob-set-object!
             grob
             'side-support-elements
             (ly:grob-list->grob-array staves)))
          grobs))
       (set! evs '())
       (set! grobs '())))))

#(ly:register-translator
 Text_mark_engraver 'Text_mark_engraver
 '((events-accepted . (text-mark-event))
   (grobs-created . (TextMark))
   (properties-read . (stavesFound))
   (properties-written . ())
   (description . "Engraves arbitrary textual marks.")))


endMark =
#(define-music-function (parser location text) (markup?)
   #{
\tweak TextMark.break-visibility #begin-of-line-invisible
\tweak TextMark.direction #DOWN
\tweak TextMark.self-alignment-X #RIGHT
\tweak TextMark.padding #2
\tweak TextMark.font-size #1
  \textEndMark\markup\italic $text #})

endMarkSmall =
#(define-music-function (parser location text) (markup?)
   #{
\tweak TextMark.break-visibility #begin-of-line-invisible
\tweak TextMark.direction #DOWN
\tweak TextMark.self-alignment-X #RIGHT
\tweak TextMark.padding #2
\tweak TextMark.font-size #0
  \textEndMark\markup\italic $text #})

beginMark =
#(define-music-function (parser location text) (markup?)
   #{
  \tweak TextMark.break-visibility #end-of-line-invisible
  \tweak TextMark.direction #UP
  \tweak TextMark.self-alignment-X #LEFT
  \tweak TextMark.padding #2
  \tweak TextMark.font-size #1
  \textMark\markup $text #})

beginMarkSmall =
#(define-music-function (parser location text) (markup?)
   #{
  \tweak TextMark.break-visibility #end-of-line-invisible
  \tweak TextMark.direction #UP
  \tweak TextMark.self-alignment-X #LEFT
  \tweak TextMark.padding #2
  \tweak TextMark.font-size #0
  \textMark\markup\italic $text #})

beginMarkDown =
#(define-music-function (parser location text) (markup?)
   #{ 
\tweak TextMark.break-visibility #end-of-line-invisible
\tweak TextMark.direction #DOWN
\tweak TextMark.self-alignment-X #LEFT
\tweak TextMark.font-size #1
\textMark\markup $text #})

%% Dacapo, segno...
segnoMark = { 
  \tweak TextMark.break-visibility #end-of-line-invisible
  \tweak TextMark.direction #UP
  \tweak TextMark.self-alignment-X #CENTER
  \textMark \markup \musicglyph #"scripts.segno"
}
segnoMarkEnd = { 
  \tweak TextMark.break-visibility #begin-of-line-invisible
  \tweak TextMark.direction #UP
  \tweak TextMark.self-alignment-X #CENTER
  \textMark \markup \musicglyph #"scripts.segno"
}
fermataMark = {
  \tweak TextMark.break-visibility #begin-of-line-invisible
  \tweak TextMark.direction #UP
  \tweak TextMark.self-alignment-X #CENTER
  \textMark\markup\larger\musicglyph#"scripts.ufermata"
}
fineMark = \endMark "Fin."
dalSegnoMark = \endMark "Dal Segno."
dacapoMark = \endMark "Da Capo."

textMarkTop = \once\override Score.TextMark.outside-staff-priority = 99999