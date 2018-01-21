#lang racket

(require pollen/setup pollen/decode txexpr)
(require "utils.rkt")
(require "styles.rkt")

(provide (all-defined-out))

(define/contract (CVheader . elements)
  (() () #:rest elements? . ->* . txexpr?)
  (case (current-poly-target)
    [(pdf ltx) (txexpr 'txt empty `("\\begin{center}\n"
                                    ,@elements
                                    "\n\\end{center}"))]
    [(html) (txexpr-ext 'div '((class "CVheader")) elements)]))

(define/contract (CVname . elements)
  (() () #:rest elements? . ->* . txexpr?)
  (let ((name (apply string-upcase elements)))
    (case (current-poly-target)
      [(pdf ltx) (txexpr-ext 'txt empty `("{\\Huge " ,name "}\n"))]
      [(html)    (txexpr-ext 'div '((class "CVname")) (list name))])))

(define/contract (CVaddress . elements)
  (() () #:rest elements? . ->* . txexpr?)
  (let ((expr (sc elements)))
    (case (current-poly-target)
      [(pdf ltx) (txexpr-ext 'txt empty `("{\\large " ,expr "}\n"))]
      [(html)    (txexpr-ext 'div '((class "CVaddress")) (list expr) )])))

(define/contract (CVphone . elements)
  (() () #:rest elements? . ->* . txexpr?)
  (case (current-poly-target)
    [(pdf ltx) (txexpr-ext 'txt empty `("{\\normalsize " ,@elements "}"))]
    [(html)    (txexpr-ext 'span '((class "CVphone")) elements)]))

(define/contract (CVemail . elements)
  (() () #:rest elements? . ->* . txexpr?)
  (let ((mail (url #:mail #t (xexpr-flatten elements))))
    (case (current-poly-target)
      [(pdf ltx) (txexpr-ext 'txt empty mail)]
      [(html)    (txexpr-ext 'span '((class "CVemail")) mail)])))

(define/contract (CVsection #:title title . elements)
  ((#:title string?) () #:rest txexpr-elements? . ->* . txexpr?)
  (let ((title (bf (list (sc title)))))
    (case (current-poly-target)
      [(pdf ltx) `(txt "\\vspace{5mm} \\hrule \\vspace{ -2mm}"
                       "\n{\\small " ,title "}"
                       "\n\n ",@elements)]
      [(html) (txexpr-ext 'div '((class "CVsection"))
                      `( ,(txexpr-ext 'hr '((class "CVsection-title")))
                         ,(txexpr-ext 'div '((class "CVsection-title")) title)
                         ,@elements))])))

(define/contract (CVobject #:period [period ""] . elements)
  (() (#:period string?) #:rest elements? . ->* . txexpr?)
  (case (current-poly-target)
    [(pdf ltx) `(txt "{\\large " ,@elements " \\hfill " ,period "}\n "
                     "\\vspace{-2mm}")]
    [(html) (txexpr-ext 'div '((class "CVobject"))
                        `( ,(txexpr-ext 'span
                                        '((class "object"))
                                        elements)
                           ,(txexpr-ext 'span
                                        '((class "period"))
                                        period)))]))

(define/contract (CVitems . elements)
  (() () #:rest elements? . ->* . txexpr?)
  (case (current-poly-target)
    [(pdf ltx) `(txt "\\begin{itemize}["
                     ;; no big margin
                     "leftmargin=*,"
                     ;; tiny bullet
                     "label=\\raisebox{0.25ex}{\\tiny$\\bullet$}]\n"
                     ,@elements
                     "\n\\end{itemize}")]
    [(html) (txexpr-ext 'ul empty elements)]))

(define/contract (CVitem #:bullet [bullet #t]
               #:tag [tag ""]
               #:tagsize [tagsize 20] . elements)
  (() (#:bullet boolean? #:tag elements? #:tagsize positive?)
      #:rest elements?
      . ->* . txexpr?)
  (case (current-poly-target)
    ;; conditionally set pieces of item
    [(pdf ltx)
     (txexpr-ext
      'txt empty
      `("\\item" ,(if bullet "" "[]") " "
                ,@(if (eq? tag "")
                      '()
                      `("\\noindent\\hbox to 0.48\\textwidth{" ,tag "\\hfill}"))
                ,@elements))]
    [(html)
     (txexpr-ext
      'li
      `((class "CVitem")
        (style ,(if bullet "" "list-style-type: none;")))
      `( ,(if (eq? tag "") ""
              ;; put tag inside a span of fixed width, for alignment
              (txexpr-ext 'span `((class "tag")
                                  (style ,(string-append
                                           "display: inline-block;")))
                          (list tag)))
         ,@elements))]))
