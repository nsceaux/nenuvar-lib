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

%%%
%%% Nomenclatures baroques  
%%%
#(make-instrument-variables "fl" "Fl" "Fl")
#(make-instrument-variables "cl" "Cl" "Cl")
#(make-instrument-variables "hb" "Hb" "Hb")
#(make-instrument-variables
  "hbdv"
  #{\markup\center-column { Hb DVn }#}
  #{\markup\center-column { Hb DVn }#})
#(make-instrument-variables
  "flhb"
  #{\markup\center-column { Fl Hb }#}
  #{\markup\center-column { Fl Hb }#})
#(make-instrument-variables
  "hbcl"
  #{\markup\center-column { Hb Cl }#}
  #{\markup\center-column { Hb Cl }#})
#(make-instrument-variables
  "fldv"
  #{\markup\center-column { Fl DVn }#}
  #{\markup\center-column { Fl DVn }#})
#(make-instrument-variables
  "flhbdv"
  #{\markup\center-column { Fl Hb DVn }#}
  #{\markup\center-column { Fl Hb DVn }#})
#(make-instrument-variables "timb" "Timb" "Timb")
#(make-instrument-variables "bn" "Bn" "Bn")
#(make-instrument-variables "dv" "DVn" "DVn")
#(make-instrument-variables "hcv" "HcVn" "HcVn")
#(make-instrument-variables "tv" "TVn" "TVn")
#(make-instrument-variables "qv" "QVn" "QVn")
#(make-instrument-variables
  "parties"
  #{\markup\center-column { HcVn TVn }#}
  #{\markup\center-column { HcVn TVn }#})
#(make-instrument-variables "basse" "B" "B")
#(make-instrument-variables "bv" "BVn" "BVn")
#(make-instrument-variables "cb" "Cb" "Cb")
#(make-instrument-variables "bc" "Bc" "Bc")
#(make-instrument-variables
  "bnb"
  #{\markup\center-column { Bn B }#}
  #{\markup\center-column { Bn B }#})
#(make-instrument-variables
  "bnbv"
  #{\markup\center-column { Bn BVn }#}
  #{\markup\center-column { Bn BVn }#})
#(make-instrument-variables
  "bnbc"
  #{\markup\center-column { Bn Bc }#}
  #{\markup\center-column { Bn Bc }#})
#(make-instrument-variables
  "bvbc"
  #{\markup\center-column { BVn Bc }#}
  #{\markup\center-column { BVn Bc }#})
#(make-instrument-variables
  "bnbvbc"
  #{\markup\center-column { Bn BVn Bc }#}
  #{\markup\center-column { Bn BVn Bc }#})
% Chœur
#(make-instrument-variables "dchant" "D" "D")
#(make-instrument-variables "dIchant" "D1" "D1")
#(make-instrument-variables "dIIchant" "D2" "D2")
#(make-instrument-variables "bdchant" "BD" "BD")
#(make-instrument-variables "hcchant" "HC" "HC")
#(make-instrument-variables "tchant" "T" "T")
#(make-instrument-variables "btchant" "BT" "BT")
#(make-instrument-variables "bchant" "B" "B")

%%%
%%% Nomenclatures classiques
%%%
#(make-instrument-variables "vn" "Vn" "Vn")
#(make-instrument-variables "va" "Alt" "Alt")
#(make-instrument-variables "vc" "Vc" "Vc")
#(make-instrument-variables "tb" "Tb" "Tb")
#(make-instrument-variables
  "vccb"
  #{\markup\center-column { Vc Cb }#}
  #{\markup\center-column { Vc Cb }#})

%%%
%%% Personnages
%%%
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
