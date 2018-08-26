%%% clef.ily  -- ancient and modern clef command
%%%
%%% Author: Nicolas Sceaux <nicolas.sceaux@gmail.com>
%%%
%%% Options
%%% =======
%%%   use-ancient-clef
%%%     When true, use ancient clefs, instead of modern ones.
%%%
%%%   show-ancient-clef
%%%     When true, show ancient clefs, before modern ones.
%%%
%%%   parenthesize-ancient-clef
%%%     When true and ancient clefs are shown before modern ones,
%%%     parenthesize the ancient clef
%%%
%%% Music functions
%%% ===============
%%%   \clef "ancient/modern"
%%%   \clef "name"
%%%
%%% Dependencies
%%% ============
%%% This feature relies on LilyPond >=2.11.40

#(use-modules (ice-9 regex))

%%%
%%% Clef mapping definition and accessors
%%%
%% actually accessors defined in a closure above
#(define (clef-map tessitura)
   "Procedure with setter, returning (ancient-clef . modern-clef) pair
for `tessitura'"
   #f)

#(let ((clef-maps '(;; instruments
                    (dessus french . treble)
                    (dessus2 petrucci-c1 . treble)
                    (haute-contre petrucci-c1 . alto)
                    (haute-contre2 petrucci-c2 . alto)
                    (taille petrucci-c2 . alto)
                    (taille2 petrucci-c3 . alto)
                    (quinte petrucci-c3 . alto)
                    (basse petrucci-f . bass)
                    ;; voix
                    (vdessus petrucci-g . treble)
                    (vbas-dessus petrucci-c1 . treble)
                    (vpetite-haute-contre petrucci-c2 . G_8)
                    (vhaute-contre petrucci-c3 . G_8)
                    (vhaute-contre2 petrucci-c3 . G_8)
                    (vtaille petrucci-c4 . G_8)
                    (vbasse-taille varbaritone . bass)
                    (vbasse petrucci-f . bass)
                    (vtenor tenor . G_8)
                    (valto  alto . treble))))
   ;; getter
   (define (clef-map-ref tessitura)
     (or (assoc tessitura clef-maps) #f))
   ;; setter
   (define (clef-map-set! tessitura clef-pair)
     (set! clef-maps (assoc-set! clef-maps tessitura clef-pair)))
   (set! clef-map (make-procedure-with-setter clef-map-ref clef-map-set!)))

%%%
%%% properties and functions for printing ancient modern clefs, side by side.
%%%
#(set-object-property! 'orig-glyph
                       'backend-type? string?)
#(set-object-property! 'orig-glyph
                       'backend-doc "Original clef glyph")
#(set-object-property! 'orig-clef-position
                       'backend-type? number?)
#(set-object-property! 'orig-clef-position
                       'backend-doc "Original clef position")

#(define (original-clef-stencil clef font-size)
   (let ((ancient-glyph
          (ly:font-get-glyph (ly:grob-default-font clef)
                             (string-append (ly:grob-property clef 'orig-glyph)
                                            "_change")))
         (factor (/ 2.0 (magstep (if (null? font-size) 0 font-size)))))
     (ly:stencil-translate-axis
      (if (eqv? #t (ly:get-option 'parenthesize-ancient-clef))
          (parenthesize-stencil ancient-glyph 0.05 0.25 0.5 0.2)
          ancient-glyph)
      (/ (- (ly:grob-property clef 'orig-clef-position)
            (ly:grob-property clef 'staff-position))
         factor)
      Y)))
   
#(define (print-clef-with-original-clef clef)
   (ly:stencil-combine-at-edge
    (original-clef-stencil
     clef
     ;; FIXME: staff-space is needed here, not font-size
     (ly:grob-property clef 'font-size))
    X RIGHT
    (ly:clef::print clef)
    ;; padding:
    0.5))

#(define (clef-modifier-with-original-clef-x-offset clef-modifier)
   (+ (ly:self-alignment-interface::x-aligned-on-self clef-modifier)
      (ly:self-alignment-interface::centered-on-x-parent clef-modifier)
      0.25 ;; padding / 2
      (* 0.5 (interval-length
              (ly:stencil-extent
               (original-clef-stencil
                (ly:grob-parent clef-modifier Y)
                (ly:grob-property clef-modifier 'font-size))
                X)))))

%%%
%%% \clef music function redefinition
%%%
clef =
#(define-music-function (parser location clef-name) (string?)
   (let* ((match (string-match "^(.*)/(.*)$" clef-name))
          (clefs (clef-map (string->symbol clef-name)))
          (ancient-clef (cond (match (match:substring match 1))
                              (clefs (symbol->string (cadr clefs)))
                              (else #f)))
          (modern-clef (cond (match (match:substring match 2))
                             (clefs (symbol->string (cddr clefs)))
                             (else clef-name))))
     (cond ((and ancient-clef (eqv? #t (ly:get-option 'use-ancient-clef)))
            ;; ancient clef only
            (make-clef-set ancient-clef))
           ((and ancient-clef (eqv? #t (ly:get-option 'show-ancient-clef)))
            ;; ancient and moden clef side by side
            (let ((clef-def (assoc ancient-clef supported-clefs)))
              (if (not (pair? clef-def))
                  (ly:error "~a is not a supported clef" ancient-clef))
              (let ((glyph (cadr clef-def))
                    (position (caddr clef-def)))
                #{
  \set Staff.forceClef = ##t
  \once\override Staff.Clef.orig-glyph = #glyph
  \once\override Staff.Clef.orig-clef-position = #position
  \once\override Staff.Clef.stencil = #print-clef-with-original-clef
  \once\override Staff.Clef.full-size-change = ##t
  \once\override Staff.ClefModifier.X-offset =
  #clef-modifier-with-original-clef-x-offset
  $(make-clef-set modern-clef) #})))
           (else
            ;; modern clef only
            (make-clef-set modern-clef)))))

ffclef =
#(define-music-function (parser location clef-name) (string?)
   #{ \set Staff.forceClef = ##t
\once\override Staff.Clef #'full-size-change = ##t
\clef $clef-name #})

fclef =
#(define-music-function (parser location clef-name) (string?)
   #{ \set Staff.forceClef = ##t \clef $clef-name #})
