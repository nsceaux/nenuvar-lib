%% Custom bar lines
\defineBarLine "!!:" #'("" "!!:" "!! ")
\defineBarLine ".|:" #'("|" ".|:" ".| ")
\defineBarLine ".!:" #'("|" ".!:" ".! ")
\defineBarLine "|!:" #'("|" "|!:" "|! ")
\defineBarLine "!:" #'("" "!:" "! ")
\defineBarLine ":!." #'(":!." "" " !.")
\defineBarLine ":!|" #'(":!|" "" " !|")
\defineBarLine ":||:" #'(":||:" "" " || ")
\defineBarLine "|;|" #'("|;|" "" "| |")
\defineBarLine " .|:" #'("" ".|:" ".| ")
\defineBarLine "|.|:" #'("|." ".|:" "|.| ")
\defineBarLine "||.!:" #'("||" ".!:" ".! ")
\defineBarLine "||.|:" #'("||" ".|:" ".| ")

%% Breathing signs from Hippolyte et Aricie
cesure = {
  \once\override BreathingSign.text = \markup \fontsize #2 "|"
  \once\override BreathingSign.Y-offset = #0
  \breathe
}
cesureCenter = {
  \once\override BreathingSign.text = \markup \fontsize #2 "|"
  \once\override BreathingSign.Y-offset = #-1
  \breathe
}
cesureDown = {
  \once\override BreathingSign.text = \markup \fontsize #2 "|"
  \once\override BreathingSign.Y-offset = #-2
  \breathe
}

cesureInstr = {
  \once\override BreathingSign.text = \markup \musicglyph #"scripts.caesura.straight"
  \breathe
}

cesureInstrCenter = {
  \once\override BreathingSign.text = \markup \musicglyph #"scripts.caesura.straight"
  \once\override BreathingSign.Y-offset = #-1
  \breathe
}
cesureInstrDown = {
  \once\override BreathingSign.text = \markup \musicglyph #"scripts.caesura.straight"
  \once\override BreathingSign.Y-offset = #-2
  \breathe
}

dotSign=\markup\vcenter "╸"

%% New baroque scripts (Charpentier, Rameau...)
#(define-public baroque-script-alist
   (append!
    `((accinv
         ; accent inversé
         (script-stencil
          . (markup . ,#{ \markup\rotate#180 \musicglyph#"scripts.sforzato" #}))
         (padding . 0.20)
         (avoid-slur . around)
         (direction . ,UP))
      (t
       (script-stencil
        . (markup . ,(markup #:center-align #:sans #:fontsize -2 #:bold "t")))
       (padding . 0.20)
       (avoid-slur . around)
       (direction . ,UP))
      (trillBaroque
        (script-stencil . (feta . ("stopped" . "stopped")))
        (side-relative-direction . ,DOWN)
        (quantize-position . #t)
        (avoid-slur . inside)
        (toward-stem-shift . 1.0)
        (toward-stem-shift-in-column . 0.0)
        (padding . 0.20)
        (skyline-horizontal-padding . 0.10)
        (script-priority . -100))
      (trillFlat
       (script-stencil
        . (markup . , #{ \markup\center-align\concat\vcenter {
                           \smaller\smaller\flat
                           \musicglyph #"scripts.stopped" } #}))
       (padding . 0.20)
       (avoid-slur . around)
       (direction . ,UP))
      (trillSharp
       (script-stencil
        . (markup . , #{ \markup\center-align\concat\vcenter {
                           \smaller\smaller\sharp
                           \musicglyph #"scripts.stopped" } #}))
       (padding . 0.20)
       (avoid-slur . around)
       (direction . ,UP))
      (trillNatural
       (script-stencil
        . (markup . , #{ \markup\center-align\concat\vcenter {
                           \smaller\smaller\natural
                           \musicglyph #"scripts.stopped" } #}))
       (padding . 0.20)
       (avoid-slur . around)
       (direction . ,UP))
      (trillSug
       (script-stencil
        . (markup . , #{ \markup\center-align\concat {
     \smaller\smaller\musicglyph #"accidentals.leftparen"
     \musicglyph #"scripts.stopped"
     \translate #'(-0.6 . 0) \smaller\smaller\musicglyph #"accidentals.rightparen"
   } #}))
        (side-relative-direction . ,DOWN)
        (quantize-position . #t)
        (avoid-slur . inside)
        (toward-stem-shift . 1.0)
        (toward-stem-shift-in-column . 0.0)
        (padding . 0.20)
        (skyline-horizontal-padding . 0.10)
        (script-priority . -100))
      (prallSug
       (script-stencil
        . (markup . , #{ \markup\center-align\concat {
     \smaller\smaller\musicglyph #"accidentals.leftparen"
     \musicglyph #"scripts.prall"
     \translate #'(-0.6 . 0) \smaller\smaller\musicglyph #"accidentals.rightparen"
   } #}))
       (padding . 0.20)
       (avoid-slur . around)
       (direction . ,UP))
      (arcTrill ; + with an arc above (like a formata with a + signe instead of dot)
       (script-stencil . (markup . , #{
     \markup\rotate#-90 \concat {
       \musicglyph#"accidentals.leftparen"
       \musicglyph#"scripts.stopped"
     }#}))
       (padding . 0.20)
       (avoid-slur . around)
       (direction . ,UP))
      (arcPrall
       (script-stencil . (markup . , #{
     \markup\rotate#-90 \concat {
       \musicglyph#"accidentals.leftparen"
       \rotate#90 \musicglyph#"scripts.prall"
     }#}))
       (padding . 0.20)
       (avoid-slur . around)
       (direction . ,UP))
      (arcDot
       (script-stencil . (markup . , #{
     \markup\rotate#-90 \concat {
       \musicglyph#"accidentals.leftparen"
       \musicglyph#"scripts.staccato"
     }#}))
       (padding . 0.40)
       (avoid-slur . around)
       (direction . ,UP))
      (arcArc
       (script-stencil
        . (markup
           . , #{
     \markup\combine\combine
     \translate #'(0 . 1) \musicglyph #"scripts.ufermata"
     \with-color #white \translate #'(0 . 1.2) \draw-circle #0.4 #0 ##t
     \combine
     \smaller\smaller\translate #'(0 . 0) \musicglyph #"scripts.ufermata"
     \with-color #white \translate #'(0 . 0.2) \draw-circle #0.4 #0 ##t
     #}))
       (padding . 0.40)
       (avoid-slur . around)
       (direction . ,UP))
      (arcArcDot
       (script-stencil
        . (markup
           . , #{
     \markup\combine\combine
     \translate #'(0 . 1) \musicglyph #"scripts.ufermata"
     \with-color #white \translate #'(0 . 1.2) \draw-circle #0.4 #0 ##t
     \smaller\smaller \translate #'(0 . 0) \musicglyph #"scripts.ufermata"
     #}))
       (padding . 0.40)
       (avoid-slur . around)
       (direction . ,UP))
      (dotDot
       (script-stencil
        . (markup . ,(markup #:center-align #:line (#:musicglyph "period" #:musicglyph "period"))))
       (padding . 0.20)
       (avoid-slur . around)
       (direction . ,UP))

       (dotThree
        (script-stencil
         . (markup . ,#{\markup\center-align {
                   \musicglyph#"scripts.staccato"
                   \musicglyph#"scripts.staccato"
                   \musicglyph#"scripts.staccato" }#}))
        (padding . 0.20)
        (avoid-slur . around)
        (direction . ,DOWN))
       (dotFour
        (script-stencil
         . (markup . ,#{\markup\center-align {
                   \musicglyph#"scripts.staccato"
                   \musicglyph#"scripts.staccato"
                   \musicglyph#"scripts.staccato"
                   \musicglyph#"scripts.staccato" }#}))
        (padding . 0.20)
        (avoid-slur . around)
        (direction . ,DOWN))

       (dotFourLig
        (script-stencil
         . (markup . ,#{ \markup\rotate#-90 \concat {
       \musicglyph#"accidentals.leftparen"
       \rotate#90 \center-align\override #'(word-space . 0.1) \line {
         \musicglyph#"scripts.staccato"
         \musicglyph#"scripts.staccato"
         \musicglyph#"scripts.staccato"
         \musicglyph#"scripts.staccato"
         }
       }#}))
        (padding . 0.40)
        (avoid-slur . around)
        (direction . ,UP))
       (dotFourLigDown
        (script-stencil
         . (markup . ,#{ \markup\rotate#90 \concat {
       \musicglyph#"accidentals.leftparen"
       \rotate#90 \center-align\override #'(word-space . 0.1) \line {
         \musicglyph#"scripts.staccato"
         \musicglyph#"scripts.staccato"
         \musicglyph#"scripts.staccato"
         \musicglyph#"scripts.staccato"
         }
       }#}))
        (padding . 0.40)
        (avoid-slur . around)
        (direction . ,DOWN))
       (prallLig
        (script-stencil
         . (markup . ,#{ \markup\rotate#-90 \concat {
                   \larger\musicglyph#"accidentals.leftparen"
                   \rotate#90 \center-align \musicglyph#"scripts.prallprall" }#}))
        (padding . 0.40)
        (avoid-slur . around)
        (direction . ,UP))

       (dotEight
        (script-stencil
         . (markup . ,#{\markup\center-align {
                   \musicglyph#"scripts.staccato"
                   \musicglyph#"scripts.staccato"
                   \musicglyph#"scripts.staccato"
                   \musicglyph#"scripts.staccato"
                   \musicglyph#"scripts.staccato"
                   \musicglyph#"scripts.staccato"
                   \musicglyph#"scripts.staccato"
                   \musicglyph#"scripts.staccato" }#}))
        (padding . 0.20)
        (avoid-slur . around)
        (direction . ,DOWN))      
       
       (dotPrall
        ; Articulation used Charpentier: a dot, followed by a prall sign
        (script-stencil . (markup . , #{
     \markup\concat\vcenter { "╸" \musicglyph#"scripts.prall" }#}))
       (padding . 0.20)
       (avoid-slur . around)
       (direction . ,UP))
      (dotDoublePrallDoublePrall
        (script-stencil . (markup . , #{
     \markup\override #'(baseline-skip . 0) \column {
       \concat\vcenter { "╸" \musicglyph#"scripts.prallprall" }
       \concat\vcenter { \transparent "╸" \musicglyph#"scripts.prallprall" }
     }#}))
       (padding . 0.20)
       (avoid-slur . around)
       (direction . ,UP))
      (doublePrall
       (script-stencil
        . (markup . ,(markup #:override '(baseline-skip . 0)
                             #:center-align #:column (#:musicglyph "scripts.prall"
                                                      #:musicglyph "scripts.prall"))))
       (padding . 0.20)
       (avoid-slur . around)
       (direction . ,UP))
      )
    default-script-alist))

#(define (baroque-script-interface::print grob)
   (let ((script-stencil (ly:grob-property grob 'script-stencil)))
     (cond ((and (pair? script-stencil)
                 (eqv? 'markup (car script-stencil)))
            (set! (ly:grob-property grob 'font-encoding) 'latin1)
            (grob-interpret-markup grob (cdr script-stencil)))
           (else
            (ly:script-interface::print grob)))))

\layout {
  \context {
    \Score
    scriptDefinitions = #baroque-script-alist
  }
  \context {
    \Voice
    \override Script.stencil = #baroque-script-interface::print
  }
}
accinv = #(make-articulation 'accinv)
trill = #(make-articulation 'trillBaroque)
trillFlat = #(make-articulation 'trillFlat)
trillSharp = #(make-articulation 'trillSharp)
trillNatural = #(make-articulation 'trillNatural)
tr = #(make-articulation 't)
trillSug = #(make-articulation 'trillSug)
prallSug = #(make-articulation 'prallSug)
arcTrill = #(make-articulation 'arcTrill)
arcPrall = #(make-articulation 'arcPrall)
arcDot = #(make-articulation 'arcDot)
arcArc = #(make-articulation 'arcArc)
arcArcDot = #(make-articulation 'arcArcDot)
dotDot = #(make-articulation 'dotDot)
dotThree = #(make-articulation 'dotThree)
dotFour = #(make-articulation 'dotFour)
dotFourLig = #(make-articulation 'dotFourLig)
dotFourLigDown = #(make-articulation 'dotFourLigDown)
dotEight = #(make-articulation 'dotEight)
dotPrall = #(make-articulation 'dotPrall)
dotDoublePrallDoublePrall = #(make-articulation 'dotDoublePrallDoublePrall)
doublePrall = #(make-articulation 'doublePrall)
prallLig = #(make-articulation 'prallLig)

%% A slur and a prall, both joined on their right ends
slurPrall = {
  \once\override Slur.direction = #UP
  \once\override Slur.text = \markup\musicglyph #"scripts.prall"
  \once\override Slur.stencil =
  #(lambda (grob)
     (let* ((slur-stencil (ly:slur::print grob))
            (coords (ly:slur::calc-control-points grob))
            (X-ext (ly:stencil-extent slur-stencil X))
            (Y-ext (ly:stencil-extent slur-stencil Y))
            (text-stencil (ly:text-interface::print grob))
            (text-width (interval-length (ly:stencil-extent text-stencil X)))
            (prall-stencil (ly:stencil-translate
                            (ly:stencil-aligned-to text-stencil X LEFT)
                            (cons (- (cdr X-ext) text-width 0.17)
                                  (+ (if (< (cdr (list-ref coords 3)) 2.8)
                                         (- 2.8 (cdr (list-ref coords 3)))
                                         0.5)
                                     (- (cdr (list-ref coords 3)) 0.15)))))
            (combo-stencil (ly:stencil-add slur-stencil prall-stencil)))
       (ly:stencil-translate combo-stencil (cons 0 0))))
  \once\override Slur.control-points =
  #(lambda (grob)
     (let* ((coords (ly:slur::calc-control-points grob))
            (point-0 (list-ref coords 0))
            (point-1 (list-ref coords 1))
            (point-2 (list-ref coords 2))
            (point-3 (list-ref coords 3))
            (text-stencil (ly:text-interface::print grob))
            (text-width (interval-length (ly:stencil-extent text-stencil X))))
       (set-cdr! point-1 (+ (cdr point-1) 1))
       (set-car! point-2 (+ (car point-2) (/ text-width 1.0)))
       (set-car! point-3 (+ (car point-3) 0.34 (/ text-width 2.0)))
       (set-cdr! point-3 (if (< (cdr point-3) 2.8)
                             2.8
                             (+ 0.5 (cdr point-3))))
       (set-cdr! point-2 (+ (cdr point-3) 2.0))
       coords))
}

%% Charpentier
%% For quarter note with eighth note flag and half note note head (in e.g. 3/2)
#(define-public (calc-white-note-head-glyph grob)
   (let ((style (ly:grob-property grob 'style))
         (duration-log (min 1 (ly:grob-property grob 'duration-log))))
     (select-head-glyph style duration-log)))

whiteNoteHeadsOn = {
  \override Staff.NoteHead.style = #'baroque
  \override Staff.NoteHead.glyph-name = #calc-white-note-head-glyph
}
whiteNoteHeadsOff = {
  \revert Staff.NoteHead.style
  \revert Staff.NoteHead.glyph-name
}

%% Black notation, for breve and whole notes
#(define-public (ly:note-head::print-black grob)
   (let ((head-style (ly:grob-property grob 'style)))
     (case head-style
       ((baroque default)
        (let* ((head-stencil (ly:note-head::print grob))
               (duration (ly:grob-property grob 'duration-log))
               (glyph-name (format #f "noteheads.s~a"
                                   (ly:grob-property grob 'glyph-name)))
               (glyph (grob-interpret-markup
                       grob
                       (make-musicglyph-markup glyph-name))))
          (ly:stencil-add
           (ly:stencil-translate
            (ly:stencil-aligned-to
             (stencil-with-color
              (if (>= duration 0)
                  ;; oval for whole note
                  (make-oval-stencil
                   (* 0.9 (/ (interval-length (ly:stencil-extent glyph X)) 2))
                   (* 1.1 (/ (interval-length (ly:stencil-extent glyph Y)) 2))
                   0 #t)
                  ;; rectangle for breve
                  (make-filled-box-stencil (ly:stencil-extent glyph X)
                                           (ly:stencil-extent glyph Y)))
              black)
             X CENTER)
            (cons (/ (interval-length (ly:stencil-extent head-stencil X)) 2)
                  0))
           head-stencil)))
       ((petrucci)
        (set! (ly:grob-property grob 'style) 'blackpetrucci)
        (ly:note-head::print grob))
       (else
        (ly:note-head::print grob)))))

blackNotation =
#(define-music-function (parser location music) (ly:music?)
   #{ \override NoteHead.stencil = #ly:note-head::print-black
      \override NoteHead.Y-extent =
      #(ly:make-unpure-pure-container
        ly:grob::stencil-height
        (lambda (grob b e) (ly:grob::stencil-height grob)))
      $music
      \revert NoteHead.stencil #})

ficta = { \once\set suggestAccidentals = ##t }

%% Nuances
tresdoux =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "très doux"))
tresdouxSug =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "[très doux]"))

doux =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.5 "doux"))
douxSug =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.5 "[doux]"))

ademi =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "a demi"))
ademiSug =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "[a demi]"))

enadoucissant =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.9
                           "en adoucissant"))
enadoucissantSug =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.9
                           "[en adoucissant]"))

plusdoux =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "plus doux"))
moinsdoux =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "moins doux"))
moinsdouxSug =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "[moins doux]"))

moinsfort =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "moins fort"))
moinsfortSug =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "[moins fort]"))

unpeufort =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "un peu fort"))

plusfort =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "plus fort"))
plusfortSug =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "[plus fort]"))

fort =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.5 "fort"))
fortSug =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.5 "[fort]"))

tresfort =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "très fort"))
tresfortSug =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "[très fort]"))
dolce =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "dolce"))
dolceSug =
#(make-music 'TextScriptEvent
             'text (markup #:whiteout #:italic #:general-align X -0.75
                           "[dolce]"))

pocof =
#(make-music 'TextScriptEvent
             'text #{ \markup\whiteout\line { \italic poco \dynamic f } #})

d = #(make-music 'AbsoluteDynamicEvent
                 'text
                 #{
\markup\combine \rotate#180 \musicglyph#"p"
\combine
\with-color#white \filled-box #'(0.9 . 1.17) #'(0.9 . 1.2) #0
\with-color#white \filled-box #'(1.63 . 2.0) #'(0.9 . 1.2) #0 #})

viste =
#(make-music
  'TextScriptEvent
  'text #{\markup\whiteout\italic\general-align #X #-0.5 viste #})
