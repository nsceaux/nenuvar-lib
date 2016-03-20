%%% tagline-and-titles.ily -- markup commands useful for taglines and titles
%%%
%%% Author: Nicolas Sceaux <nicolas.sceaux@free.fr>
%%%

%% \copyright => Copyright © [header:copyrightYear] [header:maintainer]
#(define-markup-command (copyright layout props) ()
  (let* ((maintainer (chain-assoc-get 'header:maintainer props))
         (this-year (+ 1900 (tm:year (gmtime (current-time)))))
         (year (string->number (or (chain-assoc-get 'header:copyrightYear props)
                                   (number->string this-year)))))
    (interpret-markup layout props
     (markup "Copyright ©" 
             (if (= year this-year)
                 (format #f "~a" this-year)
                 (format #f "~a-~a" year this-year))
             maintainer))))

%% \today => YYYY-MM-dd
#(define-markup-command (today layout props) ()
  (let ((today (gmtime (current-time))))
   (interpret-markup layout props
     (format #f "~a-~a-~a"
             (+ 1900 (tm:year today))
             (1+ (tm:mon today))
             (tm:mday today)))))

%% \today-french => DD mois YYYY
#(define-markup-command (today-french layout props) ()
   (let* ((date (gmtime (current-time)))
          (months '#("janvier" "février" "mars" "avril"
                               "mai" "juin" "juillet" "août"
                               "septembre" "octobre" "novembre"
                               "décembre"))
          (day (if (= (tm:mday date) 1)
                   (markup (#:concat ("1" #:super "er")))
                   (number->string (tm:mday date))))
          (month (vector-ref months (tm:mon date)))
          (year (number->string (+ 1900 (tm:year date)))))
     (interpret-markup
      layout props (markup day month year))))

#(define-markup-command (when-property layout props symbol markp) (symbol? markup?)
  (if (chain-assoc-get symbol props)
      (interpret-markup layout props markp)
      (ly:make-stencil '()  '(1 . -1) '(1 . -1))))

#(define-markup-command (apply-fromproperty layout props fn symbol)
  (procedure? symbol?)
  (let ((m (chain-assoc-get symbol props)))
    (if (markup? m)
        (interpret-markup layout props (fn m))
        empty-stencil)))

#(define-markup-command (separation-line layout props width-ratio) (number?)
   #:properties ((line-width))
   (interpret-markup
    layout props
    #{\markup\fill-line {
  \draw-line #(cons (* width-ratio line-width) 0) }#}))

#(define-markup-command (sep layout props) ()
   (interpret-markup
    layout props
    #{\markup\pad-around#1 \fill-line { \draw-line #'(50 . 0) } #}))
  