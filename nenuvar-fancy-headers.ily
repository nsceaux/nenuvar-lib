%%% fancy-headers.ily -- print fancy page headers
%%%
%%% Author: Nicolas Sceaux <nicolas.sceaux@free.fr>
%%%
%%% Services
%%% ========
%%% Define two scheme functions:
%%%   (add-odd-page-header-text  parser text display-first)
%%%   (add-even-page-header-text parser text display-first)
%%%     Set odd and even page header, respectively, to `text'.
%%%     If `display-first' is false, then `text' will not be displayed
%%%     on the first page it has been added, only on the following ones.
%%%     `parser' is the lilypond parser object.
%%%
%%% Define two markup commands:
%%%   \odd-header
%%%     Interpret the odd page headers, based on header text that have
%%%     been added using `add-odd-page-header-text'.
%%%
%%%   \even-header
%%%     Interpret the even page headers, based on header text that have
%%%     been added using `add-even-page-header-text'.
%%%
%%%  Set evenHeaderMarkup and oddHeaderMarkup \paper variables.
%%%
%%%
%%% Dependencies
%%% ============
%%% None
%%%

#(define-markup-command (page-header layout props text) (markup?)
   (let* ((page-number (chain-assoc-get 'page:page-number props -1))
          (page-number-markup (number->string page-number))
          (text-markup (markup #:italic (or text ""))))
     (if (or (= page-number 1) (not text))
         empty-stencil
         (interpret-markup layout props
                           (if (odd? page-number)
                               (markup #:fill-line (#:null text-markup page-number-markup))
                               (markup #:fill-line (page-number-markup text-markup #:null)))))))

#(define-public add-odd-page-header-text #f)
#(define-public add-even-page-header-text #f)
#(define-public in-music-add-odd-page-header-text #f)
#(define-public in-music-add-even-page-header-text #f)
#(define header-markup-aux #f)
#(let ((odd-label-header-table (list))
       (odd-page-header-table (list))
       (even-label-header-table (list))
       (even-page-header-table (list)))
  (set! header-markup-aux
   (lambda (layout props odd)
     (define (page-text page-number table)
       (if (null? table)
           ""
           (let* ((elment (car table))
                  (p (car elment))
                  (text (cadr elment))
                  (display-1st (caddr elment)))
             (cond ((and (= page-number p) (not display-1st)) #f)
                   ((>= page-number p) text)
                   (else (page-text page-number (cdr table)))))))
     (ly:make-stencil
       `(delay-stencil-evaluation
          ,(delay (ly:stencil-expr
                    (begin
                     (if (or (and odd (null? odd-page-header-table))
                             (and (not odd) (null? even-page-header-table)))
                         (let ((page-header-table (list)))
                          (for-each (lambda (label-header)
                                      (let* ((label (car label-header))
                                             (text-disp (cdr label-header))
                                             (table (ly:output-def-lookup layout 'label-page-table))
                                             (label-page (and (list? table) (assoc label table)))
                                             (page-number (and label-page (cdr label-page)))
                                             (prev-value (and page-number (assoc page-number page-header-table))))
                                        (if (not prev-value)
                                            (set! page-header-table (cons (cons page-number text-disp)
                                                                          page-header-table))
                                            (set! page-header-table
                                                  (assoc-set! page-header-table
                                                              page-number
                                                              (list (car text-disp) (caddr prev-value)))))))
                                    (reverse (if odd odd-label-header-table even-label-header-table)))
                          (if odd
                              (set! odd-page-header-table page-header-table)
                              (set! even-page-header-table page-header-table))))
                      (let ((page-number-markup (or (page-text (chain-assoc-get 'page:page-number props -1)
                                                               (if odd
                                                                   odd-page-header-table
                                                                   even-page-header-table))
                                                    "")))
                        (interpret-markup layout props
                                          (markup #:page-header page-number-markup)))))))
       (cons 0 0)
       (cons -1.0 (cdr (ly:stencil-extent (interpret-markup layout props "XXX") Y))))))
  (set! add-odd-page-header-text
   (lambda (parser text display-1st)
     (let ((label (gensym "header")))
       (set! odd-label-header-table
             (cons (list label text display-1st)
                   odd-label-header-table))
       (add-music
         (make-music 'Music
          'page-marker #t
          'page-label label)))))
  (set! in-music-add-odd-page-header-text
   (lambda (text display-1st)
     (let ((label (gensym "header")))
       (set! odd-label-header-table
             (cons (list label text display-1st)
                   odd-label-header-table))
       (make-music 'EventChord
         'page-marker #t
         'page-label label
         'elements (list (make-music 'LabelEvent 'page-label label))))))
  (set! add-even-page-header-text
   (lambda (parser text display-1st)
     (let ((label (gensym "header")))
       (set! even-label-header-table
             (cons (list label text display-1st)
                   even-label-header-table))
       (add-music
         (make-music 'Music
           'page-marker #t
           'page-label label)))))
  (set! in-music-add-even-page-header-text
   (lambda (text display-1st)
     (let ((label (gensym "header")))
       (set! even-label-header-table
             (cons (list label text display-1st)
                   even-label-header-table))
       (make-music 'EventChord
         'page-marker #t
         'page-label label
         'elements (list (make-music 'LabelEvent 'page-label label)))))))

#(define-markup-command (odd-header layout props) ()
   (header-markup-aux layout props #t))

#(define-markup-command (even-header layout props) ()
   (header-markup-aux layout props #f))

\paper {
  evenHeaderMarkup = \markup \even-header
  oddHeaderMarkup = \markup \odd-header
}

resetHeaders =
#(define-music-function (parser location) ()
   (add-even-page-header-text parser "" #f)
   (add-odd-page-header-text parser "" #f)
   (make-music 'Music 'void #t))
