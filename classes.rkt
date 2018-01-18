#lang racket/base
(require racket/format)
(require racket/string)

(require txexpr pollen/setup)

(provide (all-defined-out))

(define (CVheader . elements)
  (case (current-poly-target)
    [(pdf ltx) `(txt "\\begin{center}\n" ,@elements "\n\\end{center}")]
    [(html) (txexpr 'div '((class "CVheader")) elements)]))

(define (CVname . elements)
  (case (current-poly-target)
    [(pdf ltx) `(txt "{\\Huge " ,(apply string-upcase elements) "}\n")]
    [(html) (txexpr 'div '((class "CVname")) elements)]))

(define (CVaddress . elements)
  (case (current-poly-target)
    [(pdf ltx) `(txt "{\\large \\textsc{" ,@elements "}}\n")]
    [(html) (txexpr 'div '((class "CVaddress")) elements)]))

(define (CVphone . elements)
  (case (current-poly-target)
    [(pdf ltx) `(txt "{\\normalsize " ,@elements "}")]
    [(html) (txexpr 'span '((class "CVphone")) elements)]))

(define (CVemail . elements)
  (case (current-poly-target)
    [(pdf ltx)
     `(txt "{\\normalsize "
           "\\href{mailto:" ,@elements "}{" ,@elements "}}")]
    [(html)
     (txexpr 'span '((class "CVemail")) elements)]))

(define (CVsection #:title title . elements)
  (case (current-poly-target)
    [(pdf ltx) `(txt "\\vspace{10mm} \\hrule \\vspace{ -2mm}"
                     "\n{\\scshape \\bfseries \\small " ,title "}"
                     "\n\n ",@elements)]
    [(html) (txexpr 'span '((class "CVsection")) elements)]))

(define (CVobject #:period [period ""] . elements)
  (case (current-poly-target)
    [(pdf ltx) `(txt "{\\large " ,@elements " \\hfill " ,period "}\n")]
    [(html) (txexpr 'span '((class "CVobject")) elements)]))

(define (CVitems . elements)
  (case (current-poly-target)
    [(pdf ltx) `(txt "\\begin{itemize}["
                     ;; no big margin
                     "leftmargin=*,"
                     ;; tiny bullet
                     "label=\\raisebox{0.25ex}{\\tiny$\\bullet$}]\n"
                     ,@elements
                     "\n\\end{itemize}")]
    [(html) (txexpr 'ul empty elements)]))

(define (CVitem #:bullet [bullet #t]
               #:tag [tag #f]
               #:tagsize [tagsize 0.48] . elements)
  (case (current-poly-target)
    ;; conditionally set pieces of item
    [(pdf ltx)
     `(txt "\\item" ,(if bullet "" "[]") " "
           ,@(if tag `("\\noindent\\hbox to"
                       ,(~a tagsize) "\\textwidth{" ,tag "\\hfill}") '())
           ,@elements)]
    [(html)
     (txexpr 'li `((class "CVitem")
                   (style ,(if bullet "" "list-style-type: none;")))
             ;; merge tag with other elements
             `( ,(if tag
                     ;; apply proper attrs to tag, if any
                     (attr-set*
                      (cond
                        ;; enclose string in a x-expression
                        [(string? tag) (txexpr 'span empty (list tag))]
                        [(xexpr? tag) tag])
                      'class "tag"
                      'style (string-append
                              "display: inline-block; width: "
                              (~a (* 100 tagsize)) "%"))
                     "")
                ,@elements))]))
