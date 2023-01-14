
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Selection of version: urtext or modified
origVersion =
#(define-music-function (parser location music) (ly:music?)
   (if (eqv? #t (ly:get-option 'urtext))
       music
       (let ((type (ly:music-property music 'name)))
         (if (memq type '(TextScriptEvent ArticulationEvent TieEvent SlurEvent))
             (make-music 'TextScriptEvent 'text "")
             (make-music 'Music 'void #t)))))

modVersion =
#(define-music-function (parser location music) (ly:music?)
   (if (not (eqv? #t (ly:get-option 'urtext)))
       music
       (let ((type (ly:music-property music 'name)))
         (if (memq type '(TextScriptEvent ArticulationEvent TieEvent SlurEvent))
             (make-music 'TextScriptEvent 'text "")
             (make-music 'Music 'void #t)))))

#(define-markup-command (orig-version layout props markp) (markup?)
   (if (eqv? #t (ly:get-option 'urtext))
       (interpret-markup layout props markp)
       empty-stencil))

#(define-markup-command (mod-version layout props markp) (markup?)
   (if (not (eqv? #t (ly:get-option 'urtext)))
       (interpret-markup layout props markp)
       empty-stencil))

origLayout =
#(define-music-function (parser location music) (ly:music?)
   (if (eqv? #t (ly:get-option 'original-layout))
       music
       (make-music 'Music)))

\layout {
  \context {
    \Score
    \override NonMusicalPaperColumn.line-break-permission =
    #(if (eqv? #t (ly:get-option 'original-layout))
         #f
         'allow)
    \override NonMusicalPaperColumn.page-break-permission =
    #(if (eqv? #t (ly:get-option 'original-layout))
         #f
         'allow)
  }
}

#(define-markup-command (annotation layout props markp) (markup?)
   (if (eqv? #t (ly:get-option 'urtext))
       (interpret-markup layout props (markup #:with-color red markp))
       empty-stencil))
