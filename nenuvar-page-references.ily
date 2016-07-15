%%% page-references.ily -- page number reference markup commands
%%%
%%% Author: Nicolas Sceaux <nicolas.sceaux@free.fr>
%%%

#(define (page-ref-aux layout props label gauge next)
  (let* ((gauge-stencil
          (interpret-markup layout props
                            (make-concat-markup (list gauge next))))
         (x-ext (ly:stencil-extent gauge-stencil X))
         (y-ext (ly:stencil-extent gauge-stencil Y)))
    (ly:make-stencil
     `(delay-stencil-evaluation
       ,(delay (ly:stencil-expr
                (let* ((table (ly:output-def-lookup layout 'label-page-table))
                       (label-page (and (list? table) (assoc label table)))
                       (page-number (and label-page (cdr label-page)))
                       (page-markup (if page-number
                                        (markup #:page-link page-number
                                            #:concat ((format "~a" page-number)
                                                      next))
                                        (markup #:concat ("?" next))))
                       (page-stencil (interpret-markup layout props page-markup))
                       (gap (- (interval-length x-ext)
                               (interval-length (ly:stencil-extent page-stencil X)))))
                  (interpret-markup layout props
                                    (markup #:concat (page-markup #:hspace gap)))))))
     x-ext
     y-ext)))

#(define-markup-command (page-refI layout props label next)
  (symbol? markup?)
  (page-ref-aux layout props label "0" next))

#(define-markup-command (page-refII layout props label next)
  (symbol? markup?)
  (page-ref-aux layout props label "00" next))

#(define-markup-command (page-refIII layout props label next)
  (symbol? markup?)
  (page-ref-aux layout props label "000" next))
