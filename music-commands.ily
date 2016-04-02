\version "2.11.57"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Tagging commands
%%%

#(use-modules (srfi srfi-1))
#(define* (has-some-member? list1 list2 #:key (test eqv?))
   "Return a true value iif there exists an element of list1 that also 
belongs to list2 under test."
   (if (null? list1)
       #f
       (or (member (car list1) list2 test)
           (has-some-member? (cdr list1) list2 #:test test))))

#(define (symbol-or-symbols? x)
   (or (null? x)
       (not x)
       (symbol? x)
       (and (list? x) (every symbol? x))))

keepWithTag =
#(define-music-function (parser location tags music)
                        (symbol-or-symbols? ly:music?)
   (if tags
       (music-filter
        (lambda (m)
          (let ((m.tags (ly:music-property m 'tags)))
            (cond ((symbol? tags)
                   (or (null? m.tags) (memq tags m.tags)))
                  ((null? tags)
                   (null? m.tags))
                  ((list? tags)
                   (or (null? m.tags) (has-some-member? tags m.tags)))
                  (else #t))))
        music)
       music))

tag =
#(define-music-function (parser location tags arg)
                        (symbol-or-symbols? ly:music?)
   "Add @var{tags} (a single tag or a list of tags) to the @code{tags} 
property of @var{arg}."
   (set! (ly:music-property arg 'tags)
         (if (symbol? tags)
             (cons tags (ly:music-property arg 'tags))
             (append tags (ly:music-property arg 'tags))))
   arg)

%%% Music binding construct

setMusic =
#(define-music-function (parser location sym music) (symbol? ly:music?)
   (ly:parser-define! sym music)
   (make-music 'Music 'void #t))

%%% Two voices construct
twoVoices =
#(define-music-function (parser location tags music)
     (list? ly:music?)
   "Example:
  \\twoVoices #'(flauto1 flauto2 flauti) <<
    { music-voice1 }
    { music-voice2 }
    { optional-common-music }
  >>
=>
  <<
    \\tag #'(flauto1 flauti) \\new Voice {
      \\tag #'flauti \\voiceOne
      music-voice1
    }
    \\tag #'(flauto2 flauti) \\new Voice {
      \\tag #'flauti \\voiceTwo
      music-voice2
    }
    { optional-common-music }
  >>

Then, use:
   \\keepWithTag #'flauto1 <this-music>   for flauto1 alone
   \\keepWithTag #'flauto2 <this-music>   for flauto2 alone
   \\keepWithTag #'flauti  <this-music>   for flauto1 & flauto2
"
   (let ((tag1 (car tags))
         (tag2 (cadr tags))
         (tag-all (caddr tags))
         (music1 (car (ly:music-property music 'elements)))
         (music2 (cadr (ly:music-property music 'elements)))
         (rest-music (make-music
                      'SimultaneousMusic
                      'elements (cddr (ly:music-property music 'elements)))))
     #{ <<
  \tag #(list tag1 tag-all) \new Voice {
    \tag #tag-all \voiceOne $music1
  }
  \tag #(list tag2 tag-all) \new Voice {
    \tag #tag-all \voiceTwo $music2
  }
  \tag #(list tag1 tag2 tag-all) $rest-music
>> #}))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Repeat with alternatives
%%%

forceCloseVoltaBracket = {
  \once \override Score.VoltaBracket #'stencil =
  #(lambda (grob)
     (let* ((volta (ly:volta-bracket-interface::print grob))
            (edge-heights (ly:grob-property grob 'edge-height))
            (height (if (pair? edge-heights)
                        (car edge-heights)
                        1.5))
            (thickness (* (ly:grob-property grob 'thickness 1.6)
                          (ly:staff-symbol-line-thickness grob))))
       (ly:stencil-combine-at-edge
        volta X RIGHT
        (make-line-stencil thickness 0 0 0 (- height))
        0)))
  \once \override Score.VoltaBracket #'edge-height = #'(1.5 . 1.5)
}

alternatives =
#(define-music-function (parser location first second) (ly:music? ly:music?)
   #{ \set Score.repeatCommands = #'((volta "1."))
$first
\set Score.repeatCommands = #'((volta #f) (volta "2.") end-repeat)
$second
\set Score.repeatCommands = #'((volta #f)) #})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hideVoice = {
  \hideNotes
  \override Script #'transparent = ##t
  \override TextScript #'transparent = ##t
  \override Tie #'transparent = ##t
  \override Slur #'transparent = ##t
  \override MultiMeasureRest #'transparent = ##t
  \override AccidentalCautionary #'transparent = ##t
  \override AccidentalSuggestion #'transparent = ##t
}

unHideVoice = {
  \unHideNotes
  \revert Script #'transparent
  \revert TextScript #'transparent
  \revert Tie #'transparent
  \revert Slur #'transparent
  \revert MultiMeasureRest #'transparent
  \revert AccidentalCautionary #'transparent
  \revert AccidentalSuggestion #'transparent
}

%%%
%%% smaller notes
%%%

smallNotes =
#(define-music-function (parser location music) (ly:music?)
  (let ((first-note #f)
        (last-note #f)
        (note-count 0))
    ;; count the notes, and get first and last ones.
    (music-map
     (lambda (event)
       (if (eqv? (ly:music-property event 'name) 'NoteEvent)
           (begin
             (if first-note
                 (set! last-note event)
                 (set! first-note event))
             (set! note-count (1+ note-count))))
       event)
     music)
    ;; Add [ and ] beaming directive to the first and last note
    (if (> note-count 1)
        (begin
          (set! (ly:music-property first-note 'articulations)
                (cons (make-music 'BeamEvent 'span-direction -1)
                      (ly:music-property first-note 'articulations)))
          (set! (ly:music-property last-note 'articulations)
                (cons (make-music 'BeamEvent 'span-direction 1)
                      (ly:music-property last-note 'articulations)))))
    ;; If there are 3 notes, add a *2/3 duration factor
    (if (= note-count 3)
        (music-map
         (lambda (event)
           (if (eqv? (ly:music-property event 'name) 'NoteEvent)
               (let* ((duration (ly:music-property event 'duration))
                      (dot-count (ly:duration-dot-count duration))
                      (log (ly:duration-log duration)))
                 (set! (ly:music-property event 'duration)
                       (ly:make-duration log dot-count 2 3))))
           event)
         music)))
  #{
  \override Voice.NoteHead.font-size = #-3
  \override Voice.Flag.font-size = #-3
  \override Voice.Dots.font-size = #-3
  \override Voice.Stem.font-size = #-3
  \override Voice.Stem.length-fraction = #0.8
  \override Voice.Beam.beam-thickness = #0.384
  \override Voice.Beam.length-fraction = #0.8
  \override Voice.Accidental.font-size = #-4
  \override Voice.AccidentalCautionary.font-size = #-4
  $music
  \revert Voice.NoteHead.font-size
  \revert Voice.Flag.font-size
  \revert Voice.Dots.font-size
  \revert Voice.Stem.font-size
  \revert Voice.Stem.length-fraction
  \revert Voice.Beam.beam-thickness
  \revert Voice.Beam.length-fraction
  \revert Voice.Accidental.font-size
  \revert Voice.AccidentalCautionary.font-size
  #})

sugNotes =
#(define-music-function (parser location music) (ly:music?)
  #{
  \override Rest.font-size = #-3
  \override NoteHead.font-size = #-3
  \override Flag.font-size = #-3
  \override Dots.font-size = #-3
  \override Stem.font-size = #-3
  \override Stem.length-fraction = #0.8
  \override Beam.beam-thickness = #0.384
  \override Beam.length-fraction = #0.8
  \override Accidental.font-size = #-4
  \override AccidentalCautionary.font-size = #-4
  $music
  \revert Rest.font-size
  \revert NoteHead.font-size
  \revert Flag.font-size
  \revert Dots.font-size
  \revert Stem.font-size
  \revert Stem.length-fraction
  \revert Beam.beam-thickness
  \revert Beam.length-fraction
  \revert Accidental.font-size
  \revert AccidentalCautionary.font-size
  #})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Custos note heads
%%%

custosNote = 
#(define-music-function (parser location note) (ly:music?)
  (make-music 'SequentialMusic
   'elements (list #{ 
              \once \override Voice.NoteHead #'stencil = #ly:text-interface::print
              \once \override Voice.NoteHead #'text =
              #(markup #:null #:raise 0.0 #:musicglyph "custodes.mensural.u0")
              \once \override Voice.Flag #'stencil = ##f
              \once \override Voice.Stem #'stencil = ##f #}
              note)))

%%%
%%% Note formatting tweaks
%%%

forceStemLength = 
#(define-music-function (parser location length music) (number? ly:music?)
  #{
  \override Voice.Stem #'details = #`((lengths . (,length))
                                      (beamed-lengths . (,(- length 1.0)))
                                      (beamed-minimum-free-lengths . (,(- length 1.0)))
                                      (beamed-extreme-minimum-free-lengths . (,(- length 1.0)))
                                      (stem-shorten . (1.0 0.5)))
  $music
  \revert Voice.Stem #'details
  #})

shiftOnce = { \once \override NoteColumn #'horizontal-shift = #1 }

shiftNote =
#(define-music-function (parser location amount) (number?)
   #{ \once \override NoteHead #'X-offset = #amount
      \once \override Stem #'X-offset = #amount
      \once \override Beam #'X-offset = #amount #})

shiftRest =
#(define-music-function (parser location amount) (number?)
   #{ \once \override Rest #'X-offset = #amount #})

outside = { \once\override Staff.TextScript.outside-staff-priority = #9999 }
%%%
%%% Misc utilities
%%%

altTime =
#(define-music-function (parser location fractions) (list?)
   (define (make-time-sig-markup num den . rest)
     (if den
         (make-center-column-markup
          (list (number->string num)
                (number->string den)))
         (make-raise-markup -1 (number->string num))))

   (let ((time1 (apply make-time-sig-markup fractions))
         (time2 (apply make-time-sig-markup (cddr fractions))))
     #{
       \once \override Staff.TimeSignature #'stencil = #ly:text-interface::print
       \once \override Staff.TimeSignature #'text =
       \markup \override #'(baseline-skip . 0)
       \number \line { $time1 $time2 }
     #}))
   
fractionTime = \once\override Score.TimeSignature.style = #'numbered
cTime = \once\override Score.TimeSignature.style = #'C
digitTime = \once\override Score.TimeSignature.style = #'single-digit

instrumentName =
#(define-music-function (parser location name) (markup?)
   #{ \set Staff.instrumentName = \markup \large $name #})

characterName =
#(define-music-function (parser location name) (markup?)
  #{ \set Staff . instrumentName = \markup \large \smallCaps $name #})

midiTempo =
#(define-music-function (parser location quater-nb-par-min) (number?)
   #{ \set Score . tempoWholesPerMinute =
      #(ly:make-moment (/ quater-nb-par-min 4) 1 0 1) #})

%%%
%%% Figured bass
%%%
%% figure extenders
figExtOn = \bassFigureExtendersOn
figExtOff = \bassFigureExtendersOff

figPosOn = {
  \bassFigureExtendersOn
  \override BassFigureContinuation #'stencil = ##f
  \override Staff.BassFigureContinuation #'stencil = ##f
}
figPosOff = {
  \bassFigureExtendersOff
  \revert BassFigureContinuation #'stencil
  \revert Staff.BassFigureContinuation #'stencil
}

%% geometric figures

#(define-markup-command (triangle-up layout props a b c) (markup? markup? markup?)
   (let ((base (interpret-markup layout props (markup #:tiny #:line (#:number b #:number c))))
         (top (interpret-markup layout props (markup #:tiny #:number a))))
     (let* ((base-width (interval-length (ly:stencil-extent base X)))
            (top-width (interval-length (ly:stencil-extent top X)))
            (top-left-padding (/ (- base-width top-width) 2.0)))
       (stack-lines DOWN 0.0 2
                    (list
                     (stack-stencil-line 0 (list (ly:make-stencil "" `(0 . ,top-left-padding) '(0 . 0))
                                                 top))
                     base)))))

#(define-markup-command (triangle-down layout props a b c) (markup? markup? markup?)
   (let ((base (interpret-markup layout props (markup #:tiny #:line (#:number a #:number b))))
         (bottom (interpret-markup layout props (markup #:tiny #:number c))))
     (let* ((base-width (interval-length (ly:stencil-extent base X)))
            (bottom-width (interval-length (ly:stencil-extent bottom X)))
            (bottom-left-padding (/ (- base-width bottom-width) 2.0)))
       (stack-lines DOWN 0.0 2
                    (list
                     base
                     (stack-stencil-line
                      0 (list (ly:make-stencil ""
                                               `(0 . ,bottom-left-padding)
                                               '(0 . 0))
                              bottom)))))))

#(define-markup-command (triangle-down-down layout props a b c d) (markup? markup? markup? markup?)
   (let ((base (interpret-markup layout props (markup #:tiny #:line (#:number a #:number b))))
         (bottom (interpret-markup layout props (markup #:tiny #:number c)))
         (bottom2 (interpret-markup layout props (markup #:tiny #:number d))))
     (let* ((base-width (interval-length (ly:stencil-extent base X)))
            (bottom-width (interval-length (ly:stencil-extent bottom X)))
            (bottom-left-padding (/ (- base-width bottom-width) 2.0))
            (bottom2-width (interval-length (ly:stencil-extent bottom2 X)))
            (bottom2-left-padding (/ (- base-width bottom2-width) 2.0)))
       (stack-lines DOWN 0.0 2
                    (list
                     base
                     (stack-stencil-line
                      0 (list (ly:make-stencil ""
                                               `(0 . ,bottom-left-padding)
                                               '(0 . 0))
                              bottom))
                     (stack-stencil-line
                      0 (list (ly:make-stencil ""
                                               `(0 . ,bottom2-left-padding)
                                               '(0 . 0))
                              bottom2)))))))

#(define-markup-command (parallelogram-up-left layout props a b c d) (markup? markup? markup? markup?)
   "figure:
a b
 c d"
   (let ((top (interpret-markup layout props (markup #:tiny #:line (#:number a #:number b))))
         (bottom (interpret-markup layout props (markup #:tiny #:line (#:number c #:number d)))))
     (let* ((top-width (interval-length (ly:stencil-extent top X)))
            (bottom-left-padding (/ (- top-width
                                       (interval-length
                                        (ly:stencil-extent
                                         (interpret-markup layout props (markup #:tiny #:number c)) X)))
                                    2.0)))
       (stack-lines DOWN 0.0 2
                    (list
                     top
                     (stack-stencil-line
                      0
                      (list (ly:make-stencil ""
                                             `(0 . ,bottom-left-padding)
                                             '(0 . 0))))
                     bottom)))))

#(define-markup-command (square layout props a b c d) (markup? markup? markup? markup?)
   "figure:
a b
c d"
   (let ((top (interpret-markup layout props (markup #:tiny #:line (#:number a #:number b))))
         (bottom (interpret-markup layout props (markup #:tiny #:line (#:number c #:number d)))))
     (stack-lines DOWN 0.0 2 (list top bottom))))

#(define-markup-command (fig-five layout props a b c d e)
     (markup? markup? markup? markup? markup?)
   "figure:
a b
 c
d e"
   (let ((top (interpret-markup layout props
                                (markup #:tiny #:line (#:number a #:number b))))
         (center (interpret-markup layout props
                                   (markup #:tiny #:number c)))
         (bottom (interpret-markup layout props
                                   (markup #:tiny #:line (#:number d #:number e)))))
     (let* ((top-width (interval-length (ly:stencil-extent top X)))
            (center-width (interval-length (ly:stencil-extent center X)))
            (center-left-padding (/ (- top-width center-width) 2.0)))
       (stack-lines DOWN 0.0 2
                    (list
                     top
                     (stack-stencil-line
                      0 (list (ly:make-stencil ""
                                               `(0 . ,center-left-padding)
                                               '(0 . 0))
                              center))
                     bottom)))))

#(define-markup-command (figure-sharp layout props) ()
   (interpret-markup
    layout props
    (markup #:tiny #:concat (#:null #:raise 0.7 #:fontsize -2 #:sharp))))

#(define-markup-command (figure-flat layout props) ()
   (interpret-markup
    layout props
    (markup #:tiny #:concat (#:null #:raise 0.7 #:fontsize -2 #:flat))))

#(define-markup-command (figure-natural layout props) ()
   (interpret-markup
    layout props
    (markup #:tiny #:concat (#:null #:raise 0.7 #:fontsize -2 #:natural))))

#(define-markup-command (paren-sharp layout props num) (markup?)
   (interpret-markup
    layout props
    (markup #:tiny #:concat (#:null #:raise 0.2 #:line ("(" #:figure-sharp ")")
                                    #:number num ))))

#(define-markup-command (paren-flat layout props num) (markup?)
   (interpret-markup
    layout props
    (markup #:tiny #:concat (#:null #:raise 0.2 #:line ("(" #:figure-flat ")")
                                    #:number num ))))

%% repeats

ru =
#(define-music-function (parser location times music) (number? ly:music?)
   (if (eqv? #t (ly:get-option 'use-tremolo-repeat))
       (make-repeat "tremolo" times music '())
       (make-repeat "unfold" times music '())))
rt =
#(define-music-function (parser location times music) (number? ly:music?)
   (make-repeat "tremolo" times music '()))

rp =
#(define-music-function (parser location times music) (number? ly:music?)
   (make-repeat "percent" times music '()))

%% Tweak on articulations

tweakArticulation =
#(define-music-function (parser location property value music)
     (symbol? scheme? ly:music?)
   "Like \\tweak, but apply the tweak to articulation event found
found inside @var{music}."
   (if (equal? (object-property property 'backend-type?) #f)
       (begin
	 (ly:warning (_ "cannot find property type-check for ~a") property)
	 (ly:warning (_ "doing assignment anyway"))))
   (for-each (lambda (event)
               (set! (ly:music-property event 'tweaks)
                     (acons property value (ly:music-property event 'tweaks))))
             (ly:music-property music 'articulations))
   music)

verticalTweak =
#(define-music-function (parser location tweak) (list?)
   "Specify hard coded vertical spacing.  setting lilypond option
`apply-vertical-tweaks' to #f get rid off these tweaks."
   (if (eqv? #t (ly:get-option 'apply-vertical-tweaks))
       #{ \overrideProperty
Score.NonMusicalPaperColumn.line-break-system-details #tweak #}
       (make-music 'Music 'void #t)))



%%% conditional music
when =
#(define-music-function (parser location condition music) (boolean? ly:music?)
   (if condition
       music
       (make-music 'Music 'void #t)))

unless =
#(define-music-function (parser location condition music) (boolean? ly:music?)
   (if condition
       (make-music 'Music 'void #t)
       music))

%%% dessine une croix à l'emplacement de la note
#(define (make-erased-note-print print-procedure)
   (lambda (grob)
     (let ((note-head (print-procedure grob))
           (x-offset 1)
           (y-offset (if (number? (ly:grob-property grob 'staff-position))
                         (/ (ly:grob-property grob
                                              'staff-position)
                            2)
                         0))
           (thickness 0.4)
           (radius 2))
       (ly:stencil-add
        note-head
        (ly:stencil-add
         (ly:make-stencil
          `(draw-line ,thickness
                      ,(+ radius x-offset)
                      ,(- 0 radius y-offset)
                      ,(- x-offset radius)
                      ,(- radius y-offset))
          '(0 . 0)
          '(0 . 0))
         (ly:make-stencil
          `(draw-line ,thickness
                      ,(+ radius x-offset)
                      ,(- radius y-offset)
                      ,(- x-offset radius)
                      ,(- 0 radius y-offset))
          '(0 . 0)
          '(0 . 0)))))))

ratureNote = {
  \once\override NoteHead #'stencil =
  #(make-erased-note-print ly:note-head::print)
  \once\override Rest #'stencil =
  #(make-erased-note-print ly:rest::print)
}

#(define (make-erased-mmrest-print print-procedure)
   (lambda (grob)
     (let ((note-head (print-procedure grob))
           (x-offset 1)
           (y-offset (if (number? (ly:grob-property grob 'staff-position))
                         (/ (ly:grob-property grob
                                              'staff-position)
                            2)
                         0))
           (thickness 0.4)
           (radius 2))
       (ly:stencil-add
        note-head
        (ly:stencil-translate
         (ly:stencil-add
          (ly:make-stencil
           `(draw-line ,thickness
                       ,(+ radius x-offset)
                       ,(- 0 radius y-offset)
                       ,(- x-offset radius)
                       ,(- radius y-offset))
           '(0 . 0)
           '(0 . 0))
          (ly:make-stencil
           `(draw-line ,thickness
                       ,(+ radius x-offset)
                       ,(- radius y-offset)
                       ,(- x-offset radius)
                       ,(- 0 radius y-offset))
           '(0 . 0)
           '(0 . 0)))
         ;; translate the X so that it is on the bar center,
         ;; like the mm rest.
         (cons (car (ly:stencil-extent note-head X)) 0)
         )))))

ratureMmRest = {
  \once\override MultiMeasureRest #'stencil =
  #(make-erased-mmrest-print ly:multi-measure-rest::print)
}

%%% Suggested dynamics

sug =
#(define-music-function (parser loc arg) (ly:music?)
   (cond ((eqv? (ly:music-property arg 'name) 'AbsoluteDynamicEvent)
          (set! (ly:music-property arg 'text)
                #{ \markup\smaller\smaller
                   #(ly:music-property arg 'text) #}))
          ((eqv? (ly:music-property arg 'name) 'CrescendoEvent)
           (set! (ly:music-property arg 'span-text)
                 #{ \markup\smaller\smaller
                   #(ly:music-property arg 'span-text) #}))
          ((eqv? (ly:music-property arg 'name) 'SlurEvent)
           (set! (ly:music-property arg 'tweaks)
                 (cons '((Slur . dash-definition) (0 1 0.4 0.75))
                       (ly:music-property arg 'tweaks))))
          ((eqv? (ly:music-property arg 'name) 'ArticulationEvent)
           (set! (ly:music-property arg 'tweaks)
                 (cons '(font-size . -2)
                       (ly:music-property arg 'tweaks))))
          ((eqv? (ly:music-property arg 'name) 'RestEvent)
           (set! (ly:music-property arg 'tweaks)
                 (append! '(((Rest . font-size) . -3))
                       (ly:music-property arg 'tweaks))))
          ((eqv? (ly:music-property arg 'name) 'NoteEvent)
           (set! (ly:music-property arg 'tweaks)
                 (append! '(((NoteHead . font-size) . -3)
                            ((Flag . font-size) . -3)
                            ((Dots . font-size) . -3)
                            ((Stem . font-size) . -3)
                            ((Stem . length-fraction) . 0.8)
                            ((Beam . beam-thickness) . 0.384)
                            ((Beam . length-fraction) . 0.8)
                            ((Accidental . font-size) . -4)
                            ((AccidentalCautionary . font-size) . -4))
                       (ly:music-property arg 'tweaks)))))
   arg)

rinf = #(make-dynamic-script #{\markup\normal-text\italic rinf #})
