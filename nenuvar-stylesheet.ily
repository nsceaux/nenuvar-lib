#(ly:set-option 'point-and-click #f)

\layout {
  \context {
    \FiguredBass
    \override BassFigure.font-size = #2
  }
  \context {
    \Staff
    \override BassFigure.font-size = #2
  }
}
%% Paper size
#(set-default-paper-size "a4")

\paper {
  %% Margins, line width
  two-sided = ##t
  inner-margin = 15\mm
  outer-margin = 10\mm
  #(define line-width (- paper-width (* 25 mm)))
  ragged-bottom = ##f
  ragged-last-bottom = ##f
}

\layout {
  smallindent = 10\mm
  noindent = 0\mm
  largeindent = 25\mm
  hugeindent = 35\mm
  indent = \smallindent

  \context {
    \Score
    \name Score
    barNumberVisibility = #first-bar-number-invisible-save-broken-bars
    \override BarNumber.padding = #2 
    \override InstrumentName.font-size = #1.0
    \override InstrumentName.space-alist = #'((left-edge extra-space . 2.0))
    \accepts "StaffGroupNoBar"
    \accepts "StaffGroupNoBracket"
    skipBars = ##t
  }
  \context {
    \StaffGroup
    \name StaffGroup
    \accepts "StaffGroupNoBracket"
  }
  \context {
    \StaffGroup
    \type "Engraver_group"
    \name StaffGroupNoBar
    \description "Like StaffGroup, but without spanbar"
    \remove "Span_bar_engraver"
    \accepts "StaffGroupNoBracket"
    \accepts "InnerStaffGroup"
    \accepts "InnerChoirStaff"
  }
  \context {
    \StaffGroup
    \type "Engraver_group"
    \name StaffGroupNoBracket
    \description "Like StaffGroup, but without brackets"
    \remove "System_start_delimiter_engraver"
  }
  \context {
    \ChoirStaff
    \consists "Instrument_name_engraver"
  }
  \context {
    \Staff
    \name Staff
    \override VerticalAxisGroup.minimum-Y-extent = #'(-4 . 4)
    %% ancient-style: no time signature or key signature
    %% at an end of line.
    \override TimeSignature.break-visibility =
    #(if (eqv? #t (ly:get-option 'urtext))
         end-of-line-invisible
         all-visible)
    explicitKeySignatureVisibility =
    #(if (eqv? #t (ly:get-option 'urtext))
         end-of-line-invisible
         all-visible)
    explicitClefVisibility =
    #(if (eqv? #t (ly:get-option 'urtext))
         end-of-line-invisible
         all-visible)
    %% Figured bass
    ignoreFiguredBassRest = ##f
    figuredBassAlterationDirection = #RIGHT
    \override BassFigureAlignmentPositioning.direction = #DOWN
  }
  \context {
    \FiguredBass
    \name FiguredBass
    figuredBassAlterationDirection = #RIGHT
    %% Give Figured Bass an horizontal extent so that RehearsalMarks
    %% do not ignore it (otherwise, marks would be place between staff
    %% and figured bass)
    \override BassFigureAlignment.X-extent = #'(0 . 0)
  }
  \context {
    \PianoStaff
    \name PianoStaff
    \override StaffGrouper.staff-staff-spacing.stretchability = #1
  }
  \context {
    \Voice
    \name Voice
    \override AccidentalSuggestion.avoid-slur = #'outside
    \override NoteHead.style = #'baroque
  }
  \context {
    \Lyrics
    %% Répéter les tirets dans les paroles après un saut de ligne
    \override LyricHyphen.after-line-breaking = ##t
  }
}

\midi {
  \context {
    \type "Performer_group"
    \name StaffGroup
    \accepts StaffGroupNoBar
  }
  \context {
    \StaffGroup
    \name StaffGroupNoBar
    \accepts StaffGroupNoBracket
  }
  \context {
    \StaffGroup
    \name StaffGroupNoBracket
  }
}

forceGroupBracket = \override SystemStartBracket.collapse-height = #4

greyNotes = {
  \override Stem.color = #(x11-color 'grey30)
  \override Beam.color = #(x11-color 'grey30)
  \override NoteHead.color = #(x11-color 'grey30)
  \override Accidental.color = #(x11-color 'grey30)
}

%% override beaming behavior
%% in 2/2, group 16th notes by 4 (not by 8)
#(define-public (add-beam-exception time-signature beam-exception settings)
   (assoc-set! settings
               time-signature
               (assoc-set!
                (assoc-get time-signature settings '())
                'beamExceptions
                (cons beam-exception
                      (beam-exceptions time-signature settings)))))
#(set! default-time-signature-settings
       (add-beam-exception
        '(2 . 2) '(end ((1 . 16) 4 4 4 4))
        (copy-tree default-time-signature-settings)))

%%% Title page, headers and footers

#(define-markup-command (tagline-vspacer layout props) ()
   (interpret-markup
    layout props
    #{ \markup\abs-fontsize #10 \with-color #white \char ##x01C0 #}))

\header {
  maintainer = "Nicolas Sceaux"
  maintainerEmail = "nicolas.sceaux@gmail.com"
  maintainerWeb = "https://editions-nicolas-sceaux.fr"
  copyright = \markup\copyright
  license = "Creative Commons Attribution-ShareAlike 4.0 License"
  shortcopyright = \markup { \copyright — \license }
  longcopyright = \markup\column {
    \vspace #1
    \fill-line { \copyright }
    \fill-line { \license }
  }
  
  tagline = \markup\sans\abs-fontsize #8 \override #'(baseline-skip . 0) {
    \right-column\bold {
      \with-url #"https://editions-nicolas-sceaux.fr" {
        \concat { Éditions \tagline-vspacer }
        \concat { Nicolas \tagline-vspacer }
        \concat { Sceaux \tagline-vspacer }
      }
    }
    \abs-fontsize #9 \with-color #(x11-color 'grey40) \raise #-0.7 \musicglyph #"clefs.petrucci.f"
    \column {
      \line { \tagline-vspacer \copyright }
      \smaller\line {
        \tagline-vspacer
        Sheet music from
        \with-url #"https://editions-nicolas-sceaux.fr"
        https://editions-nicolas-sceaux.fr
        typeset using \with-url #"http://lilypond.org" LilyPond
        on \concat { \today . }
      }
      \smaller\line {
        \tagline-vspacer \license
        — free to download, distribute, modify and perform.
      }
    }
  }
}

#(define-markup-command (nenuvar-footer layout props side)
     (number?)
   (interpret-markup
    layout props
    (cond ((and (= 1 (chain-assoc-get 'page:page-number props -1))
                (not (and (chain-assoc-get 'page:is-bookpart-last-page
                                           props #f)
                          (chain-assoc-get 'page:is-last-bookpart
                                           props #f))))
           ;; Book first page
           #{ \markup\sans\fill-line { \fromproperty #'header:tagline } #})
          ((and (chain-assoc-get 'page:is-bookpart-last-page props #f)
                (chain-assoc-get 'page:is-last-bookpart props #f))
           ;; book last page
           #{ \markup\sans\fill-line { \fromproperty #'header:tagline } #})
          ((= side LEFT)
           ;; even pages
           #{ \markup\sans\fill-line {
  \null \abs-fontsize #6 \fromproperty #'header:shortcopyright } #})
          (else
           ;; odd pages
           #{ \markup\sans\fill-line {
  \abs-fontsize #6 \fromproperty #'header:shortcopyright \null } #}))))

\paper {
  nenuvarBookTitleMarkup = \markup \when-property #'header:title \abs-fontsize #12 \column {
    \null \null \null \null \null \null
    \fill-line { \fontsize #6 \italic \fromproperty #'header:composer }
    \when-property #'header:poet \column { \null \null \null }
    \fill-line { \fontsize #6 \italic \fromproperty #'header:poet }
    \null \null \null \null \null \null
    \fontsize #12 \fill-line {
      \apply-fromproperty #make-smallCaps-markup #'header:title
    }
    \null
    \when-property #'header:subtitle \fontsize#6 \fill-line { \fromproperty #'header:subtitle }
    \null \null \null \null \null
    \separation-line#0.2
    \null \null \null \null \null \null
    \fill-line { \fontsize #4 \fromproperty #'header:opus }
    \null
    \fill-line { \fontsize #4 \fromproperty #'header:date }
    \null
    \on-the-fly #(lambda (layout props arg)
                   (if (*part*)
                       (interpret-markup layout props
                         (markup #:column (#:null #:null
                                           #:fill-line (#:fontsize 4 (*part-name*)))))
                       empty-stencil))
    \null \null \null \null
    \fill-line { \fontsize #2 \fromproperty #'header:editions }
    \fill-line { \fontsize #2 \fromproperty #'header:arrangement }
  }
  bookTitleMarkup = \nenuvarBookTitleMarkup
  shortBookTitleMarkup =  \markup\abs-fontsize #12 {
    \override #'(baseline-skip . 3.5) \column {
      \fontsize#3 \bold \fill-line { \larger \fromproperty #'header:title }
      \fontsize#0 \fill-line { \fromproperty #'header:subtitle }
      \fill-line {
        \column {
          \fromproperty #'header:poet
          \fromproperty #'header:opus
        }
        \on-the-fly #(lambda (layout props arg)
                      (if (*part*)
                       (interpret-markup layout props (markup (*part-name*)))
                       empty-stencil)) \null
        \right-column {
          \fromproperty #'header:composer
          \fromproperty #'header:date
        }
      }
    }
  }
  scoreTitleMarkup = #f

  oddFooterMarkup = \markup\nenuvar-footer #RIGHT
  evenFooterMarkup = \markup\nenuvar-footer #LEFT

  tocTitle = "TABLE DES MATIÈRES"
}
