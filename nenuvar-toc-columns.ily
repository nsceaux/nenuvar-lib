%%% toc-columns.ily -- Table of content on several columns
%%%
%%% Author: Nicolas Sceaux <nicolas.sceaux@free.fr>
%%%

%% Add optional arguments to `add-toc-item!'
#(let ((toc-item-list (list)))
   (set! add-toc-item!
         (lambda (markup-symbol text . rest)
           (let ((label (gensym "toc")))
             (set! toc-item-list
                   (cons (append! (list label markup-symbol text) rest)
                         toc-item-list))
             (make-music 'EventChord
               'page-marker #t
               'page-label label
               'elements (list (make-music 'LabelEvent
                                 'page-label label))))))
   (set! toc-items (lambda ()
                     (reverse toc-item-list))))

%% Add dot-filling option to \line and \wordwrap
#(define-markup-command (fromproperty layout props sym) (symbol?)
   (let ((m (chain-assoc-get sym props)))
     (cond ((string? m)
            (interpret-markup layout props (make-simple-markup m)))
           ((markup? m)
            (if (memq (car m) (list wordwrap-markup))
                (interpret-markup layout props m)
                (interpret-markup layout props (make-line-markup (list m)))))
           (else
            empty-stencil))))

#(define-markup-command (line layout props args) (markup-list?)
  #:properties ((word-space)
                (line-width #f)
                (text-direction RIGHT)
                (fill-with-dots #f))
   "Like built-in @code{line}, but fill the line
with dots in property @code{fill-with-dots} is true."
   (let* ((props (cons `((fill-with-dots . #f)) props))
          (line (let ((stencils (interpret-markup-list layout props args)))
                  (if (= text-direction LEFT)
                      (set! stencils (reverse stencils)))
                  (stack-stencil-line
                   word-space
                   (remove ly:stencil-empty? stencils)))))
     (if (not fill-with-dots)
         line
         (interpret-markup layout props
                           (markup #:fill-with-pattern 0.5 RIGHT "."
                                   #:stencil line
                                   #:null)))))

#(define-markup-command (simple layout props str) (string?)
  #:properties ((fill-with-dots #f))
   "Like built-in @code{simple}, but fill the line
with dots in property @code{fill-with-dots} is true."
   (if fill-with-dots
       (interpret-markup layout props (make-line-markup (list str)))
       (interpret-markup layout props str)))

#(define-markup-command (wordwrap layout props args) (markup-list?)
   #:properties ((baseline-skip 0.3)
                 (fill-with-dots #f)
                 wordwrap-internal-markup-list)
   "Like built-in @code{wordwrap}, but fill the last line
with dots in property @code{fill-with-dots} is true."
   (let* ((no-dots-props (cons `((fill-with-dots . #f)) props))
          (lines (space-lines
                  baseline-skip
                  (wordwrap-internal-markup-list layout no-dots-props #f args))))
     (stack-lines DOWN 0 0
                  (space-lines
                   baseline-skip
                   (if (or (null? lines) (not fill-with-dots))
                       lines
                       (let* ((reversed-lines (reverse! lines)))
                         (reverse! (cons (interpret-markup
                                          layout props
                                          (make-line-markup
                                           (list (make-stencil-markup (car reversed-lines)))))
                                         (cdr reversed-lines)))))))))

%% stencil utilities
#(define (combine-left stencil . rest)
   (cond ((null? rest)
          stencil)
         ((ly:stencil-empty? (car rest))
          (apply combine-left stencil (cdr rest)))
         (else
          (apply combine-left
                 ; ly:stencil-combine-at-edge first axis direction second padding
                 (ly:stencil-combine-at-edge stencil X RIGHT (car rest) 0)
                 (cdr rest)))))

#(define (space-stencil width)
   (ly:make-stencil "" (cons 0 width) (cons 0 0)))

#(define-markup-command (toc-filled-line layout props rehearsal-number text page)
     (markup? markup? markup?)
   #:properties ((line-width #f)
                 (word-space 0)
                 (baseline-skip 0.3)
                 (fill-line-with-dots #t)
                 (use-rehearsal-numbers #f)
                 (rehearsal-number-gauge "8-88")
                 (rehearsal-number-align RIGHT)
                 (rehearsal-number-margin 1)
                 (page-number-gauge "000")
                 (page-number-margin 1))
   (let* ((line-width (or line-width (ly:output-def-lookup layout 'line-width)))
          ;; page number
          (page-number-stencil
           (let* ((bare-page-number-stencil (interpret-markup layout props page))
                  (left-padding (max 0
                                     (- (interval-length
                                         (ly:stencil-extent
                                          (interpret-markup layout props page-number-gauge)
                                          X))
                                        (interval-length
                                         (ly:stencil-extent bare-page-number-stencil X))))))
             (combine-left (space-stencil page-number-margin)
                           (space-stencil left-padding)
                           bare-page-number-stencil)))
          (page-number-width (interval-length
                              (ly:stencil-extent page-number-stencil X)))
          ;; rehearsal numbers: set later if actually used
          (num-width 0)
          (num-stencil empty-stencil))
     ;; If rehearsal number is printed, compute its width and stencil
     (if use-rehearsal-numbers
         (let* ((bare-num-stencil (interpret-markup layout props rehearsal-number))
                (bare-width (interval-length (ly:stencil-extent bare-num-stencil X)))
                (num-gauge-stencil (interpret-markup layout props rehearsal-number-gauge))
                (gauge-width (interval-length (ly:stencil-extent num-gauge-stencil X)))
                (padding (max 0 (- gauge-width bare-width)))
                (right-padding (* (/ (1+ (* -1 rehearsal-number-align)) 2.0) padding))
                (left-padding (- padding right-padding)))
           (set! num-stencil
                 (combine-left
                  ; left padding
                  (space-stencil left-padding)
                  ; rehearsal-number
                  bare-num-stencil
                  ; right padding
                  (space-stencil right-padding)
                  ; margin
                  (space-stencil rehearsal-number-margin)))
           (set! num-width (interval-length
                            (ly:stencil-extent num-stencil X)))))
     ;; compute text width and stencil
     (let* ((text-max-width (- line-width page-number-width num-width))
            (text-stencil (interpret-markup
                           layout props
                           (markup #:override `(fill-with-dots . ,fill-line-with-dots)
                                   #:override `(line-width . ,text-max-width)
                                   text)))
            (y-offset (min 0 (+ (cdr (ly:stencil-extent page-number-stencil Y))
                                (- (interval-length (ly:stencil-extent page-number-stencil Y))
                                   (interval-length (ly:stencil-extent text-stencil Y)))))))
       (combine-left num-stencil
                     text-stencil
                     (ly:stencil-translate-axis page-number-stencil y-offset Y)))))

#(define-markup-command (paper-prop layout props name default)
  (symbol? markup?)
  "Get the value of a \\paper property, or defaults to some value"
  (let ((val (ly:output-def-lookup layout name)))
    (interpret-markup layout props (if (markup? val)
                                      val
                                      default))))

\paper {
  tocTitleMarkup = \markup\column {
    \vspace #2
    \fontsize #6 \fill-line { \paper-prop #'tocTitle "TABLE OF CONTENTS" }
    \vspace #2
  }
  tocActMarkup = \markup\large\italic\column {
    \vspace #1
    \fontsize #2 \fill-line { \fromproperty #'toc:text }
    \vspace #1
  }
  tocSceneMarkup = \markup\override #'(baseline-skip . 0) \column {
    \vspace#0.3
    \override #'(fill-line-with-dots . #f) \toc-filled-line
    "" \larger\fromproperty #'toc:text ""
  }
  tocPieceMarkup = \markup {
    \toc-filled-line
    \sans\fromproperty #'toc:rehearsal-number
    \fromproperty #'toc:text
    \fromproperty #'toc:page
  }
  tocBoldPieceMarkup = \markup {
    \toc-filled-line
    \fromproperty #'toc:rehearsal-number
    \bold\fromproperty #'toc:text
    \fromproperty #'toc:page
  }
  tocBreakMarkup = \markup\column-break
  tocFillerMarkup = \markup\fromproperty #'toc:text
}

#(define-markup-command (toc-item layout props toc-item) (list?)
   #:properties ((section-markup 'tocActMarkup)
                 (column-number 2)
                 (inter-column-padding 5)
                 (line-width #f)
                 (baseline-skip 0.3))
   (let ((label (car toc-item))
         (toc-markup (cadr toc-item))
         (text (caddr toc-item))
         (num (if (null? (cdddr toc-item))
                  '()
                  (cadddr toc-item)))
         (column-width (/ (- line-width
                             (* (- column-number 1) inter-column-padding))
                          column-number)))
     (car (space-lines
           baseline-skip
           (list (interpret-markup
                  layout
                  (cons `((line-width . ,column-width)
                          (toc:page . ,(markup #:with-link label
                                               #:page-ref label "XXX" "?"))
                          (toc:rehearsal-number . ,num)
                          (toc:text . ,text))
                        props)
                  (ly:output-def-lookup layout toc-markup)))))))

#(define-markup-list-command (table-of-contents layout props) ()
   #:properties ((column-number 2)
                 (line-width #f)
                 (baseline-skip 0.3)
                 (column-padding 3)
                 (estimated-page-number 2)
                 ;; extra margins in mm:
                 (extra-top-padding 2)
                 (extra-bottom-padding 3))
   (interpret-markup-list
    layout
    props
    (make-page-columns-helper-markup-list
     ;; use title
     #t
     ;; the TOC title
     (ly:output-def-lookup layout 'tocTitleMarkup)
     ;; TOC lines
     (map (lambda (toc-item)
            (markup #:stencil
                    (interpret-markup layout props (markup #:toc-item toc-item))))
          (toc-items)))))
