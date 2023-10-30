%% Select EB Garamond font (with a magnification factor)

#(define-public (use-garamond-font factor)
  (let ((n (make-font-tree-node 'font-encoding 'fetaMusic)))
    (add-music-fonts n 'feta "emmentaler" "emmentaler" feta-design-size-mapping factor)
    (cond ((eq? (ly:get-option 'backend) 'svg)
           (add-pango-fonts n 'roman "serif" factor)
           (add-pango-fonts n 'sans "sans-serif" factor)
           (add-pango-fonts n 'typewriter "monospace" factor))
          (else
           (add-pango-fonts n 'roman "EB Garamond" (* 1.18 factor))
           (add-pango-fonts n 'sans "LilyPond Sans Serif" factor)
           (add-pango-fonts n 'typewriter "LilyPond Monospace" factor)))
    n))

\paper {
  #(define fonts (use-garamond-font (/ staff-height pt 20)))
}

%% EB Garamond has a small caps feature => use it
%% Exception: æ small capsis not supported. Use hack

#(define-markup-command (smallCaps layout props text) (markup?)
   (interpret-markup
    layout props
    (if (string? text)
        (let ((parts (string-split text (integer->char 230))))
          (if (null? (cdr parts))
              (make-override-markup '(font-features . ("smcp")) text)
              (make-concat-markup
               (cons (make-override-markup '(font-features . ("smcp")) (car parts))
                     (reverse!
                      (fold (lambda (next-part prev-parts)
                              (cons (make-override-markup '(font-features . ("smcp")) next-part)
                                    (cons (make-fontsize-markup -2 "Æ")
                                          prev-parts)))
                            '()
                            (cdr parts)))))))
        (make-override-markup '(font-features . ("smcp")) text))))

