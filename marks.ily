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


endMark =
#(define-music-function (parser location text) (markup?)
   #{
  \once\override Score.RehearsalMark.break-visibility =
  #begin-of-line-invisible
  \once\override Score.RehearsalMark.direction = #DOWN
  \once\override Score.RehearsalMark.self-alignment-X = #RIGHT
  \once\override Score.RehearsalMark.padding = #2
  \once\override Score.RehearsalMark.font-size = #1
  \mark\markup\right-align\italic $text #})

endMarkSmall =
#(define-music-function (parser location text) (markup?)
   #{
  \once\override Score.RehearsalMark.break-visibility =
  #begin-of-line-invisible
  \once\override Score.RehearsalMark.direction = #DOWN
  \once\override Score.RehearsalMark.self-alignment-X = #RIGHT
  \once\override Score.RehearsalMark.padding = #2
  \once\override Score.RehearsalMark.font-size = #0
  \mark\markup\right-align\italic $text #})

beginMark =
#(define-music-function (parser location text) (markup?)
   #{
  \once\override Score.RehearsalMark.break-visibility =
  #end-of-line-invisible
  \once\override Score.RehearsalMark.direction = #UP
  \once\override Score.RehearsalMark.self-alignment-X = #LEFT
  \once\override Score.RehearsalMark.padding = #2
  \once\override Score.RehearsalMark.font-size = #1
  \mark\markup $text #})

beginMarkSmall =
#(define-music-function (parser location text) (markup?)
   #{
  \once\override Score.RehearsalMark.break-visibility =
  #end-of-line-invisible
  \once\override Score.RehearsalMark.direction = #UP
  \once\override Score.RehearsalMark.self-alignment-X = #LEFT
  \once\override Score.RehearsalMark.padding = #2
  \once\override Score.RehearsalMark.font-size = #0
  \mark\markup\italic $text #})

beginMarkDown =
#(define-music-function (parser location text) (markup?)
   #{ 
\once\override Score.RehearsalMark.break-visibility =
#end-of-line-invisible
\once\override Score.RehearsalMark.direction = #DOWN
\once\override Score.RehearsalMark.self-alignment-X = #LEFT
\once\override Score.RehearsalMark.font-size = #1
\mark\markup $text #})
