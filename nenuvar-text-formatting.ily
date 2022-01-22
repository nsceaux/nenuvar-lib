%%% text-formmating.ily -- text formatting markup commands
%%%
%%% Author: Nicolas Sceaux <nicolas.sceaux@free.fr>
%%%

%%%
%%% Redefinition of \column, \justify and \wordwrap
%%% to fix spacing around blocks
%%%

%% \column { markups }
#(define-markup-command (column layout props args) (markup-list?)
   #:properties ((baseline-skip))
   (let ((arg-stencils (interpret-markup-list layout props args)))
     (stack-lines DOWN 0.0 0
                  (space-lines baseline-skip
                               (remove ly:stencil-empty? arg-stencils)))))

%% \justify { markups }
#(define-markup-command (justify layout props args)
     (markup-list?)
   #:properties ((baseline-skip)
		 wordwrap-internal-markup-list)
   (stack-lines
    DOWN 0.0 0
    (space-lines baseline-skip
                 (wordwrap-internal-markup-list layout props #t args))))

%% \wordwrap { markups }
#(define-markup-command (wordwrap layout props args)
     (markup-list?)
   #:properties ((baseline-skip)
		 wordwrap-internal-markup-list)
  (stack-lines
   DOWN 0.0 0
   (space-lines baseline-skip
	        (wordwrap-internal-markup-list layout props #f args))))

%% \wordwrap-center { markups }
#(define-markup-command (wordwrap-center layout props args) (markup-list?)
  (interpret-markup layout props
   (make-column-markup
    (make-wordwrap-center-lines-markup-list args))))

%%%
%%% markup-list commands
%%%

%% \paragraph { markups }
#(define-markup-list-command (paragraph layout props text) (markup-list?)
  (let ((indentation (markup #:pad-to-box (cons 0 3) (cons 0 0) #:null)))
    (interpret-markup-list layout props
       (make-justified-lines-markup-list (cons indentation text)))))

%% \indented-lines { markups }
#(define-markup-list-command (indented-lines layout props indent args)
  (number? markup-list?)
  (let* ((new-line-width (- (chain-assoc-get 'line-width props) indent))
         (lines (interpret-markup-list layout
                 (cons `((line-width . ,new-line-width)) props)
                 args)))
   (interpret-markup-list layout props
    (map (lambda (line)
          (markup #:hspace indent #:stencil line))
     lines))))

%% \wordwrap-center-lines { markups }
#(define-markup-list-command (wordwrap-center-lines layout props args)
  (markup-list?)
  (map (lambda (stencil)
        (interpret-markup layout props (markup #:fill-line (#:stencil stencil))))
   (interpret-markup-list layout props (make-wordwrap-lines-markup-list args))))

%% \centered-lines { markups }
#(define-markup-list-command (centered-lines layout props args)
  (markup-list?)
  (let ((baseline-skip (chain-assoc-get 'baseline-skip props)))
    (space-lines baseline-skip
      (interpret-markup-list layout props
        (map (lambda (arg) (markup #:fill-line (arg)))
             args)))))

#(define-markup-list-command (two-column-lines layout props col1 col2)
   (markup-list? markup-list?)
   (interpret-markup-list
    layout props
    (make-column-lines-markup-list
     (let ((result '()))
       (let map-on-lists ((col1 col1)
                          (col2 col2))
         (if (and (null? col1) (null? col2))
             (reverse! result)
             (let ((line-col1 (if (null? col1) "" (car col1)))
                   (line-col2 (if (null? col2) "" (car col2)))
                   (rest-col1 (if (null? col1) '() (cdr col1)))
                   (rest-col2 (if (null? col2) '() (cdr col2))))
               (set! result (cons
                             (markup #:fill-line
                                     (#:null
                                      #:force-line-width-ratio 0.45 line-col1
                                      #:null
                                      #:force-line-width-ratio 0.45 line-col2
                                      #:null))
                             result))
               (map-on-lists rest-col1 rest-col2))))))))

%%%
%%% line width setting commands
%%%

%% \force-line-width-ratio number markup
#(define-markup-command (force-line-width-ratio layout props ratio arg)
     (number? markup?)
   (let* ((new-line-width (* ratio (chain-assoc-get 'line-width props)))
          (line-stencil (interpret-markup
                         layout props
                         (markup #:override (cons 'line-width new-line-width)
                                 arg)))
          (gap (max 0
                    (- new-line-width
                       (interval-length (ly:stencil-extent line-stencil X))))))
     (interpret-markup layout props (markup #:concat (#:stencil line-stencil #:hspace gap)))))

%% \with-line-width-ratio number { markups }
#(define-markup-list-command (with-line-width-ratio layout props width-ratio args)
  (number? markup-list?)
  (let* ((line-width (chain-assoc-get 'line-width props))
         (new-line-width (* width-ratio line-width))
         (indent (* 0.5 (- line-width new-line-width)))
         (stencils (interpret-markup-list layout
                     (cons `((line-width . ,new-line-width)) props)
                     args)))
    (interpret-markup-list layout props
      (map (lambda (stencil)
             (markup #:hspace indent #:stencil stencil))
           stencils))))

%%%
%%% Font size markup list commands
%%%

%% \fontsize-lines number { markups }
#(define-markup-list-command (fontsize-lines layout props increment args)
   (number? markup-list?)
   #:properties ((font-size 0)
                 (word-space 1)
                 (baseline-skip 2))
   (interpret-markup-list layout
                          (cons `((baseline-skip . ,(* baseline-skip (magstep increment)))
                                  (word-space . ,(* word-space (magstep increment)))
                                  (font-size . ,(+ font-size increment)))
                                props)
                          args))

%% \abs-fontsize-lines number { markups }
#(define-markup-list-command (abs-fontsize-lines layout props size args)
  (number? markup-list?)
  (let* ((ref-size (ly:output-def-lookup layout 'text-font-size 12))
         (text-props (list (ly:output-def-lookup layout 'text-font-defaults)))
         (ref-word-space (chain-assoc-get 'word-space text-props 0.6))
         (ref-baseline (chain-assoc-get 'baseline-skip text-props 3))
         (magnification (/ size ref-size)))
    (interpret-markup-list layout
                           (cons `((baseline-skip . ,(* magnification ref-baseline))
                                   (word-space . ,(* magnification ref-word-space))
                                   (font-size . ,(magnification->font-size magnification)))
                                 props)
                           args)))

%%%
%%% Useful generic text markup commands
%%%

#(define-markup-command (super layout props arg) (markup?)
  (ly:stencil-translate-axis
   (interpret-markup
    layout
    (cons `((font-size . ,(- (chain-assoc-get 'font-size props 0) 3))) props)
    arg)
   (* 0.25 (chain-assoc-get 'baseline-skip props))
   Y))

%%% Guile does not deal with accented letters
#(use-modules (ice-9 regex))
%%;; actually defined below, in a closure
#(define-public string-upper-case #f)
#(define accented-char-upper-case? #f)
#(define accented-char-lower-case? #f)

%%;; an accented character is seen as two characters by guile
#(let ((lower-case-accented-string "éèêëáàâäíìîïóòôöúùûüçœæ")
       (upper-case-accented-string "ÉÈÊËÁÀÂÄÍÌÎÏÓÒÔÖÚÙÛÜÇŒÆ"))
   (define (group-by-2 chars result)
      (if (or (null? chars) (null? (cdr chars)))
          (reverse! result)
          (group-by-2 (cddr chars)
                      (cons (string (car chars) (cadr chars))
                            result))))
   (let ((lower-case-accented-chars
          (group-by-2 (string->list lower-case-accented-string) (list)))
         (upper-case-accented-chars
          (group-by-2 (string->list upper-case-accented-string) (list))))
     (set! string-upper-case
           (lambda (str)
             (define (replace-chars str froms tos)
               (if (null? froms)
                   str
                   (replace-chars (regexp-substitute/global #f (car froms) str
                                                            'pre (car tos) 'post)
                                  (cdr froms)
                                  (cdr tos))))
             (string-upcase (replace-chars str
                                           lower-case-accented-chars
                                           upper-case-accented-chars))))
     (set! accented-char-upper-case?
           (lambda (char1 char2)
             (member (string char1 char2) upper-case-accented-chars string=?)))
     (set! accented-char-lower-case?
           (lambda (char1 char2)
             (member (string char1 char2) lower-case-accented-chars string=?)))))

#(define-markup-command (smallCaps layout props text) (markup?)
  "Turn @code{text}, which should be a string, to small caps.
@example
\\markup \\small-caps \"Text between double quotes\"
@end example"
  (define (string-list->markup strings lower)
    (let ((final-string (string-upper-case
                         (apply string-append (reverse strings)))))
      (if lower
          (markup #:fontsize -2 final-string)
          final-string)))
  (define (make-small-caps rest-chars currents current-is-lower prev-result)
    (if (null? rest-chars)
        (make-concat-markup (reverse! (cons (string-list->markup
                                              currents current-is-lower)
                                            prev-result)))
        (let* ((ch1 (car rest-chars))
               (ch2 (and (not (null? (cdr rest-chars))) (cadr rest-chars)))
               (this-char-string (string ch1))
               (is-lower (char-lower-case? ch1))
               (next-rest-chars (cdr rest-chars)))
          (cond ((and ch2 (accented-char-lower-case? ch1 ch2))
                 (set! this-char-string (string ch1 ch2))
                 (set! is-lower #t)
                 (set! next-rest-chars (cddr rest-chars)))
                ((and ch2 (accented-char-upper-case? ch1 ch2))
                 (set! this-char-string (string ch1 ch2))
                 (set! is-lower #f)
                 (set! next-rest-chars (cddr rest-chars))))
          (if (or (and current-is-lower is-lower)
                  (and (not current-is-lower) (not is-lower)))
              (make-small-caps next-rest-chars
                               (cons this-char-string currents)
                               is-lower
                               prev-result)
              (make-small-caps next-rest-chars
                               (list this-char-string)
                               is-lower
                               (if (null? currents)
                                   prev-result
                                   (cons (string-list->markup
                                            currents current-is-lower)
                                         prev-result)))))))
  (interpret-markup layout props
    (if (string? text)
        (make-small-caps (string->list text) (list) #f (list))
        text)))

#(define-markup-command (pad-above layout props amount arg)
  (number? markup?)
   (let* ((m (interpret-markup layout props arg))
          (x (ly:stencil-extent m X))
          (y (let ((extent (ly:stencil-extent m Y)))
               (cons (car extent) (+ (cdr extent) amount)))))
     (ly:stencil-add (make-transparent-box-stencil x y)
                     m)))


%% Brackets
#(define (other-axis a)
   (remainder (+ a 1) 2))

#(define-public (single-bracketify-stencil stil axis direction thick protrusion padding)
  "Add brackets around @var{stil}, producing a new stencil."
  (let* ((ext (ly:stencil-extent stil axis))
         (bracket (ly:bracket axis ext thick (* -1 direction protrusion))))
    (set! stil
          (ly:stencil-combine-at-edge
            stil (other-axis axis) direction bracket padding))
    stil))

#(define-markup-command (left-bracket layout props arg)
  (markup?)
  (let ((th 0.1)
        (m (interpret-markup layout props arg)))
    (single-bracketify-stencil m Y LEFT th (* 2.5 th) th)))

#(define-markup-command (right-bracket layout props arg)
  (markup?)
  (let ((th 0.1)
        (m (interpret-markup layout props arg)))
    (single-bracketify-stencil m Y RIGHT th (* 2.5 th) th)))

%% Lyrics commands
italicLyrics = \override Lyrics.LyricText.font-shape = #'italic
normalLyrics = \revert Lyrics.LyricText.font-shape
