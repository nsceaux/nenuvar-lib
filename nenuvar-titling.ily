%%% titling.ily -- commands for defining titles
%%%
%%% Author: Nicolas Sceaux <nicolas.sceaux@free.fr>
%%%
%%% Options
%%% =======
%%%   use-rehearsal-numbers
%%%     If #t, add rehearsal numbers to piece titles.
%%%
%%%   part
%%%     If a symbol, then the score is considered a separate part
%%%
%%% Scheme functions
%%% ================
%%%
%%%   (add-page-break parser)
%%%     add a page break at the current point.
%%%
%%%   (add-no-page-break parser)
%%%     forbid page break at current point.
%%%
%%%   (add-toplevel-markup parser text)
%%%     add a markup at current point.
%%%
%%%   (add-toc-item parser markup-symbol text [rehearsal-number])
%%%     add an item in the table of content, using markup style
%%%     `markup-symbol' and `text', referencing the page occuring
%%%     at current point.  Argument `rehearsal-number' is optional
%%%
%%%   (rehearsal-number)
%%%     return a new x.y rehearsal number.
%%%
%%%   (increase-rehearsal-major-number)
%%%     increase the major part of rehearsal numbers (ie the x in x.y)
%%%     and reset the minor part.
%%%
%%% Markup commands
%%% ===============
%%%
%%%   \act <title>
%%%     Markup command to be used for act titles
%%%
%%%   \scene <title>
%%%     Markup command to be used for scene titles
%%%
%%%   \title <title>
%%%     Markup command to be used for piece titles
%%%
%%%   \scene-desription <markup>
%%%     Markup command to be used for entering a scene description.
%%%
%%% Table of contexts \paper variables
%%% ==================================
%%%
%%%   tocTitle
%%%     a string used as the table of contents title
%%%
%%%   tocPieceMarkup
%%%     markup used for pieces
%%%
%%%   tocSceneMarkup
%%%     markup used for scene titles
%%%
%%%   tocActMarkup
%%%     markup used for act titles
%%%
%%% Music functions
%%% ===============
%%% Piece titling:
%%%
%%%   \pieceToc <title-markup>
%%%     add a piece title in the table of contents.
%%%
%%%   \pieceTocTitle <title-string>
%%%     add the piece title in the table of contents and at current 
%%%     point of the book (upper cased).
%%%
%%%   \pieceTitle <title-string>
%%%     add the upper cased piece title at the current point of the book.
%%%
%%%   \pieceTocAndTitle <toc-markup> <title-markup>
%%%     add a piece title in the table of content, and a different
%%%     title at the current point of the book.
%%%
%%% Sectionning:
%%%
%%%   \opusTitle <title-string>
%%%
%%%   \ouverture <title-string>
%%%   \act <title-string>
%%%   \scene <title-string>
%%%   \sceneDescription <description-markup>
%%%
%%%   \actEnd
%%%     Print an act end text.
%%%
%%% Dependencies
%%% ============
%%% \include "fancy-headers.ily"
%%% \include "includes.ily"
%%% \include "toc-columns.ily"
%%% \include "text-formatting.ily"

%%%
%%% Utilities for adding (no-)page breaks and toplevel markups
%%%

#(define-public (add-page-break parser)
  (add-music
   (make-music 'Music
	       'page-marker #t
	       'line-break-permission 'force
	       'page-break-permission 'force)))

#(define-public (add-no-page-break parser)
  (add-music
   (make-music 'Music
	       'page-marker #t
	       'page-break-permission 'forbid)))

#(define-public (add-no-page-turn parser)
  (add-music
   (make-music 'Music
	       'page-marker #t
	       'page-turn-permission 'forbid)))

#(define-public (add-page-turn parser)
  (add-music
   (make-music 'Music
	       'page-marker #t
	       'line-break-permission 'force
	       'page-break-permission 'force
	       'page-turn-permission 'force)))

#(define-public (add-allow-page-turn parser)
  (add-music
   (make-music 'Music
	       'page-marker #t
	       'page-turn-permission 'allow
               'elements (list (make-music 'PageTurnEvent
                                           'break-permission 'allow)))))

#(define-public (add-toplevel-markup parser text)
  (add-text text))

#(define-public (add-toc-item parser markup-symbol text . rest)
  (add-music
   (apply add-toc-item! markup-symbol text rest)))

%%%
%%% Rehearsal numbers
%%%

#(define-public rehearsal-number #f)
#(define-public increase-rehearsal-major-number #f)
#(let ((major-number 0)
       (minor-number 0))
  (set! increase-rehearsal-major-number
        (lambda ()
          (set! major-number (1+ major-number))
          (set! minor-number 0)))
  (set! rehearsal-number
        (lambda ()
          (set! minor-number (1+ minor-number))
          (format #f "~a-~a" major-number minor-number))))

%%%
%%% Markup commands
%%%

#(define-markup-command (rehearsal-number layout props text) (string?)
   (interpret-markup
    layout props
    #{ \markup\sans\huge\bold $text #}))

#(define-markup-command (act layout props arg) (markup?)
   (interpret-markup
    layout props
    (if (symbol? (ly:get-option 'part))
        ;; separate part
        #{ \markup\pad-markup#2 \fill-line { \fontsize#6 $arg } #}
          ;; lead-sheet
          #{ \markup\column { 
    \vspace#3
    \pad-markup#3 \fill-line { \fontsize#6 $arg } } #})))
    
#(define-markup-command (scene layout props arg) (markup?)
   (interpret-markup
    layout props
    (if (symbol? (ly:get-option 'part))
        #{\markup\pad-markup#0.5 \fill-line { \fontsize#4 $arg } #}
          #{\markup\column {
    \vspace#1
    \fill-line { \fontsize#4 $arg }
    \vspace#1 } #})))

#(define-markup-command (scene-description layout props arg) (markup?)
   (interpret-markup
    layout props
    (if (symbol? (ly:get-option 'part))
        empty-markup
        #{\markup\column {
  \fill-line { \override#'(line-width . 80) \fontsize#2 $arg }
  \vspace#1 } #})))

#(define-markup-command (title layout props arg) (markup?)
   (interpret-markup
    layout props
    #{\markup\fill-line { \override#'(line-width . 80) \fontsize#2 $arg } #}))

#(define-markup-command (small-title layout props arg) (markup?)
   (interpret-markup
    layout props
    #{\markup\fill-line { \override#'(line-width . 80) \italic $arg } #}))
  
#(define-markup-command (piece-title-with-rehearsal-number
                         layout props rehearsal-num title)
     (markup? markup?)
   (interpret-markup
    layout props
    #{ \markup { \rehearsal-number $rehearsal-num \hspace#1 \huge $title }#}))

#(define-markup-command (piece-title-without-rehearsal-number
                         layout props title)
     (markup?)
   (interpret-markup
    layout props
    #{ \markup\huge $title #}))

%%%
%%% Table of contents: see toc-columns.ily
%%% the following paper markup variables are supposed to be defined:
%%%  tocPieceMarkup tocSceneMarkup tocActMarkup
%%%

%%%
%%% Music functions
%%%

tocItem =
#(define-music-function (parser location title) (markup?)
   (add-toc-item parser 'tocPieceMarkup title)
   (make-music 'Music 'void #t))

%%% Pieces
#(define (add-piece-toc-and-title parser rehearsal-num title toc-title)
   (let ((rehearsal (or rehearsal-num (rehearsal-number))))
     (add-toc-item parser 'tocPieceMarkup toc-title rehearsal)
     (add-toplevel-markup
      parser
      (if (eqv? #t (ly:get-option 'use-rehearsal-numbers))
          #{ \markup\piece-title-with-rehearsal-number $rehearsal $title #}
          #{ \markup\piece-title-without-rehearsal-number $title #}))
     (add-no-page-break parser)))

pieceToc =
#(define-music-function (parser location title) (markup?)
   (add-piece-toc-and-title parser #f title title)
   (make-music 'Music 'void #t))

pieceTocCond =
#(define-music-function (parser location condition title) (boolean? markup?)
   (if condition
       (add-piece-toc-and-title parser #f title title))
   (make-music 'Music 'void #t))

pieceTocNb =
#(define-music-function (parser location number title) (string? markup?)
   (add-piece-toc-and-title parser number title title)
   (make-music 'Music 'void #t))

pieceTitleToc =
#(define-music-function (parser location title toc-title) (markup? markup?)
   (add-piece-toc-and-title parser #f title toc-title)
   (make-music 'Music 'void #t))

pieceTitleTocCond =
#(define-music-function (parser location condition title toc-title)
     (boolean? markup? markup?)
   (if condition
       (add-piece-toc-and-title parser #f title toc-title))
   (make-music 'Music 'void #t))

pieceTitleTocNb =
#(define-music-function (parser location number title toc-title)
     (string? markup? markup?)
   (add-piece-toc-and-title parser number title toc-title)
   (make-music 'Music 'void #t))

pieceTitleTocNbCond =
#(define-music-function (parser location condition number title toc-title)
     (boolean? string? markup? markup?)
   (if condition
       (add-piece-toc-and-title parser number title toc-title))
   (make-music 'Music 'void #t))

inMusicPieceToc =
#(define-music-function (parser location title) (markup?)
  (let ((rehearsal (rehearsal-number)))
    (add-toc-item! 'tocPieceMarkup title rehearsal)))

%%%
%%% Sectionning
%%%
#(use-modules (srfi srfi-39))
#(define-public *opus-title* (make-parameter ""))
#(define-public *act-title* (make-parameter ""))

%% Editorial notes
notesSection =
#(define-music-function (parser location title) (markup?)
  (add-page-break parser)
  (add-toc-item parser 'tocPieceMarkup title)
  (add-even-page-header-text parser (string-upper-case (*opus-title*)) #f)
  (*act-title* title)
  (add-odd-page-header-text parser (string-upper-case (*act-title*)) #f)
  (make-music 'Music 'void #t))

opusTitle =
#(define-music-function (parser location title) (string?)
   (*opus-title* (if (symbol? (ly:get-option 'part))
                     (string-append title ", " (*part-name*))
                     title))
   (make-music 'Music 'void #t))

act =
#(define-music-function (parser location act-title) (string?)
  (increase-rehearsal-major-number)
  (add-toc-item parser 'tocActMarkup act-title)
  (add-even-page-header-text parser (string-upper-case (*opus-title*)) #f)
  (*act-title* act-title)
  (add-odd-page-header-text parser (string-upper-case (*act-title*)) #f)
  (add-toplevel-markup parser #{\markup\act $(string-upper-case act-title)#})
  (add-no-page-break parser)
  (make-music 'Music 'void #t))

actToc =
#(define-music-function (parser location act-title) (string?)
  (increase-rehearsal-major-number)
  (add-toc-item parser 'tocActMarkup act-title)
  (add-even-page-header-text parser (string-upper-case (*opus-title*)) #f)
  (*act-title* act-title)
  (add-odd-page-header-text parser (string-upper-case (*act-title*)) #f)
  (make-music 'Music 'void #t))

actn =
#(define-music-function (parser location act-title) (string?)
  (add-toc-item parser 'tocActMarkup act-title)
  (add-even-page-header-text parser (string-upper-case (*opus-title*)) #f)
  (*act-title* act-title)
  (add-odd-page-header-text parser (string-upper-case (*act-title*)) #f)
  (add-toplevel-markup parser #{\markup\act $(string-upper-case act-title)#})
  (add-no-page-break parser)
  (make-music 'Music 'void #t))

actnToc =
#(define-music-function (parser location act-title) (string?)
  (add-toc-item parser 'tocActMarkup act-title)
  (add-even-page-header-text parser (string-upper-case (*opus-title*)) #f)
  (*act-title* act-title)
  (add-odd-page-header-text parser (string-upper-case (*act-title*)) #f)
  (make-music 'Music 'void #t))

actEnd =
#(define-music-function (parser location text) (markup?)
  (add-no-page-break parser)
  (add-toplevel-markup parser #{\markup\fontsize#4 \fill-line { $text } #})
  (add-no-page-break parser)
  (add-toplevel-markup parser #{\markup\null #})
  (make-music 'Music 'void #t))

scene =
#(define-music-function (parser location title toc-title) (string? markup?)
   (add-toc-item parser 'tocSceneMarkup
                 (if (and (string? toc-title)
                          (string-null? toc-title))
                     (string-upper-case title)
                     toc-title))
  (add-odd-page-header-text
    parser
    (format #f "~a, ~a."
           (string-upper-case (*act-title*))
           (string-upper-case title))
    #t)
  (add-toplevel-markup parser #{\markup\scene $(string-upper-case title)#})
  (add-no-page-break parser)
  (make-music 'Music 'void #t))

inMusicScene =
#(define-music-function (parser location title toc-title) (string? markup?)
   (add-toc-item parser 'tocSceneMarkup toc-title)
   (let ((label-music
          (make-music
           'SimultaneousMusic
           'elements (list (in-music-add-odd-page-header-text
                            (format #f "~a, ~a."
                                    (string-upper-case (*act-title*))
                                    (string-upper-case title))
                            #t)))))
     #{ $label-music
        \once\override Score.RehearsalMark.font-size = #0
        \once\override Score.RehearsalMark.self-alignment-X = #LEFT
        \mark\markup\fontsize#4 $(string-upper-case title) #}))

inMusicSceneDescCond =
#(define-music-function (parser location cond title toc-title description)
     (boolean? string? markup? markup?)
   (if cond
       (begin
         (add-toc-item parser 'tocSceneMarkup toc-title)
         (let ((label-music
                (make-music
                 'SimultaneousMusic
                 'elements (list (in-music-add-odd-page-header-text
                                  (format #f "~a, ~a."
                                          (string-upper-case (*act-title*))
                                          (string-upper-case title))
                                  #t))))
               (description-markup (if (symbol? (ly:get-option 'part))
                                       empty-markup
                                       (markup #:fontsize 2 description))))
           #{ $label-music
              \once\override Score.RehearsalMark.font-size = #0
              \once\override Score.RehearsalMark.self-alignment-X = #LEFT
              \mark\markup\left-align\center-column {
                \fontsize#4 $(string-upper-case title)
                \vspace#1
                $description-markup
              } #}))
       (make-music 'Music 'void #t)))

inMusicSceneDesc =
#(define-music-function (parser location title description) (string? markup?)
   (let ((label-music
          (make-music
           'SimultaneousMusic
           'elements (list (in-music-add-odd-page-header-text
                            (format #f "~a, ~a."
                                    (string-upper-case (*act-title*))
                                    (string-upper-case title))
                            #t)
                           (add-toc-item! 'tocSceneMarkup title ""))))
         (description-markup (if (symbol? (ly:get-option 'part))
                                 empty-markup
                                 (markup #:fontsize 2 description))))
     #{ $label-music
        \once\override Score.RehearsalMark.font-size = #0
        \once\override Score.RehearsalMark.self-alignment-X = #LEFT
        \mark\markup\left-align\center-column {
          \fontsize#4 $(string-upper-case title)
          \vspace#1
          $description-markup
        } #}))

sceneDescription =
#(define-music-function (parser location description) (markup?)
   (if (not (symbol? (ly:get-option 'part)))
       (begin
         (add-toplevel-markup
          parser #{\markup\scene-description $description #})
         (add-no-page-break parser)))
   (make-music 'Music 'void #t))

sceneDescriptionBottom =
#(define-music-function (parser location description) (markup?)
  (add-toplevel-markup parser #{\markup\scene-description $description #})
  (add-allow-page-turn parser)
  (make-music 'Music 'void #t))

%%% Conditionnal page breaks
partPageBreak =
#(define-music-function (parser location parts) (list?)
  (if (memq (ly:get-option 'part) parts)
      (add-page-break parser))
  (make-music 'Music 'void #t))

partNoPageBreak =
#(define-music-function (parser location parts) (list?)
  (if (memq (ly:get-option 'part) parts)
      (add-no-page-break parser))
  (make-music 'Music 'void #t))

partBlankPageBreak =
#(define-music-function (parser location parts) (list?)
  (if (memq (ly:get-option 'part) parts)
      (begin
       (add-page-break parser)
       (add-toplevel-markup parser (markup #:null))
       (add-page-break parser)))
  (make-music 'Music 'void #t))

pageBreakCond =
#(define-music-function (parser location condition) (boolean?)
  (if condition
      (add-page-break parser))
  (make-music 'Music 'void #t))

partAllowPageTurn =
#(define-music-function (parser location parts) (list?)
  (if (memq (ly:get-option 'part) parts)
      (add-allow-page-turn parser))
  (make-music 'Music 'void #t))

partNoPageTurn =
#(define-music-function (parser location parts) (list?)
  (if (or (and (symbol? (ly:get-option 'part)) (null? parts))
          (memq (ly:get-option 'part) parts))
      (add-no-page-turn parser))
  (make-music 'Music 'void #t))

partPageTurn =
#(define-music-function (parser location parts) (list?)
  (if (memq (ly:get-option 'part) parts)
      (add-page-turn parser))
  (make-music 'Music 'void #t))

markupCond =
#(define-music-function (parser location condition markp) (boolean? markup?)
   (if condition
       (add-toplevel-markup parser markp))
   (make-music 'Music 'void #t))
