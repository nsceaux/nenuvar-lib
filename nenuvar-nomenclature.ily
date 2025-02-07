#(define (make-instrument-variables name full-name short-name)
     ;; \instrumentInstr
     (ly:parser-define! (string->symbol (format #f "~(~a~)Instr" name))
       #{
\with {
  instrumentName = $full-name
  shortInstrumentName = $short-name
} #})
     ;; \instrumentInstrSug
     (ly:parser-define! (string->symbol (format #f "~(~a~)InstrSug" name))
       #{
\with {
  instrumentName = $(format #f "[~a]" short-name)
  shortInstrumentName = $(format #f "[~a]" short-name)
} #}))

#(make-instrument-variables "fl" "Fl" "Fl")
#(make-instrument-variables "hb" "Hb" "Hb")
#(make-instrument-variables
  "hbdv"
  #{\markup\center-column { Hb DVn }#}
  #{\markup\center-column { Hb DVn }#})
#(make-instrument-variables "bn" "Bn" "Bn")
#(make-instrument-variables "dv" "DVn" "DVn")
#(make-instrument-variables "hcv" "HcVn" "HcVn")
#(make-instrument-variables "tv" "TVn" "TVn")
#(make-instrument-variables "qv" "QVn" "QVn")
#(make-instrument-variables "basse" "B" "B")
#(make-instrument-variables "bv" "BVn" "BVn")
#(make-instrument-variables "bc" "Bc" "Bc")
#(make-instrument-variables
  "bvbc"
  #{\markup\center-column { BVn Bc }#}
  #{\markup\center-column { BVn Bc }#})
% Chœur
#(make-instrument-variables "dchant" "D" "D")
#(make-instrument-variables "dIchant" "D1" "D1")
#(make-instrument-variables "dIIchant" "D2" "D2")
#(make-instrument-variables "bdchant" "BD" "BD")
#(make-instrument-variables "hcchant" "HC" "HC")
#(make-instrument-variables "tchant" "T" "T")
#(make-instrument-variables "btchant" "BT" "BT")
#(make-instrument-variables "bchant" "B" "B")

#(define (make-character-variables name mark-name instr-name clef)
     ;; \characterInstr
     (ly:parser-define! (string->symbol (format #f "~aInstr" name))
       #{
\with {
  instrumentName = \markup $instr-name
  shortInstrumentName = \markup $instr-name
} #})
     ;; \characterClef
     (ly:parser-define! (string->symbol (format #f "~aClef" name))
       (define-music-function () ()
       #{ \ffclef $clef #}))
     ;; \characterName
     (ly:parser-define! (string->symbol (format #f "~aName" name))
       (define-music-function () ()
       #{ <>^\markup\character $mark-name #}))
     ;; \characterMark
     (ly:parser-define! (string->symbol (format #f "~aMark" name))
       (define-music-function () ()
       #{
  \ffclef $clef <>^\markup\character $mark-name
  \unless #(symbol? (*part*)) \set Staff.shortInstrumentName = \markup $instr-name
          #}))
     ;; \characterMarkText
     (ly:parser-define! (string->symbol (format #f "~aMarkText" name))
       (define-music-function (text) (string?)
         #{
  \ffclef $clef <>^\markup\character-text $mark-name $text
  \unless #(symbol? (*part*)) \set Staff.shortInstrumentName = \markup $instr-name #})))
