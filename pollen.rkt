#lang pollen/mode racket/base
(require racket/string racket/format)
(require racket/contract)

(require pollen/setup pollen/decode txexpr)

(module setup racket/base
  (provide (all-defined-out))
  (define poly-targets '(html ltx pdf)))

(provide (all-defined-out))

;; define the type of pollen arguments
(define (elements? el)
  ((or/c string? txexpr? (listof elements?)) el))

(define/contract (xexpr-flatten . elements)
  (() () #:rest elements? . ->* . string?)
  (define (xexpr->flat-string expr)
    (cond
      [(string? expr) expr]
      [(xexpr? expr) (string-append (xexpr->flat-string (get-elements expr)))]
      [(list? expr) (apply string-append (map xexpr->flat-string expr))]
      [else ""]))
  (xexpr->flat-string elements))

(define/contract (txexpr-ext tag [attrs empty] [elements '()])
  ((txexpr-tag?) (txexpr-attrs? elements?) . ->* . txexpr?)
  (let ((fixed-elements
         (cond
           [(txexpr-elements? elements) elements]
           [(xexpr? elements) (list elements)]
           [(string? elements) (list elements)]
           [(and (list? elements)
                 (= 1 (length elements))) (car elements)]
           [else elements])))
    (txexpr tag attrs fixed-elements)))

(define/contract (url #:link [link ""] #:mail [mail #f] . elements)
  ((elements?) (#:link string? #:mail boolean?) . ->* . txexpr?)
  (let* (;; override default link (content) if requested
         (uri (if (non-empty-string? link) link elements))
         ;; flatten uri, to remove style commands
         (uri (xexpr-flatten uri))
         ;; add mailto if requested
         (uri (string-append (if mail "mailto:" "") uri)))
    (case (current-poly-target)
      [(pdf ltx) (txexpr-ext 'txt empty `("\\href{" ,uri  "}{" ,@elements "}"))]
      [(html) (txexpr-ext 'a `((href ,uri)) elements)])))

(define-syntax-rule (define-style latex-style html-style)
  ;; style names uses latex convention by default
  (define/contract (latex-style . elements)
    (elements? . -> . txexpr?)
    (let* ((name (symbol->string 'latex-style)))
      (case (current-poly-target)
        [(pdf ltx) (txexpr-ext (string->symbol (~a "text" name))
                              empty elements)]
        [(html) (txexpr-ext 'span '((style html-style)) elements)]))))

(define-style sc "font-variant:   small-caps;")
(define-style bf "font-weight:    bold;")
(define-style ac "text-transform: uppercase;")
(define-style sl "font-style:     oblique;")
(define-style it "font-style:     italic;")

(define/contract (tex-C++ str)
  (string? . -> . string?)
  (case (current-poly-target)
    [(html) str]
    [(pdf ltx)
     (string-replace str "C++"
                     (string-append "C\\kern-0.2ex "
                                    "\\raise .1ex \\hbox{+\\kern-0.4ex +}"))]))

(define (root . items)
  (decode (txexpr-ext 'body '() items)
          #:txexpr-elements-proc (lambda (x)
                                   (case (current-poly-target)
                                     [(html) (decode-paragraphs x)]
                                     [else x]))

          #:string-proc (lambda (x)
                          (case (current-poly-target)
                            [(html) ((compose1 smart-quotes smart-dashes) x)]
                            [else (tex-C++ x)]))))

(define (->LaTeX x)
  (cond
    [(txexpr? x)
     (let* ((tag (get-tag x))
            (elements (get-elements x))
            ;; need to filter, see input (txt txt "spam")
            (content (apply string-append
                            (filter string? (map ->LaTeX elements)))))
       (case tag
         [(body root txt) (~a content)] ;; ignore "service" tags
         [else (format "\\~a{~a}" tag content)]))]
    [else x]))

(define (make-latex-source doc)
  ◊string-append{
                 \documentclass[a4paper, 12pt]{letter}
                 \usepackage[top=3cm,
                             bottom=3cm,
                             left=3cm,
                             right=3cm]{geometry}

                 \usepackage[english]{babel}
                 \usepackage[utf8x]{inputenc}
                 \usepackage[T1]{fontenc}
                 \usepackage{charter}
                 \usepackage[hidelinks]{hyperref}
                 \usepackage{enumitem}
                 \usepackage{graphicx}
                 \pagenumbering{gobble}

                 \begin{document}
                 ◊(->LaTeX doc)
                 \end{document}
                 })

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
      [(pdf ltx) `(txt "\\vspace{10mm} \\hrule \\vspace{ -2mm}"
                       "\n{\\small " ,title "}"
                       "\n\n ",@elements)]
      [(html) (txexpr-ext 'div '((class "CVsection"))
                      `( ,(txexpr-ext 'hr '((class "CVsection-title")))
                         ,(txexpr-ext 'div '((class "CVsection-title")) title)
                         ,@elements))])))

(define/contract (CVobject #:period [period ""] . elements)
  (() (#:period string?) #:rest elements? . ->* . txexpr?)
  (case (current-poly-target)
    [(pdf ltx) `(txt "{\\large " ,@elements " \\hfill " ,period "}\n")]
    [(html) (txexpr-ext 'div '((class "CVobject"))
                    `( ,(txexpr-ext 'span empty elements)
                       ,(txexpr-ext 'span
                                    '((style "float: right;"))
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
     `(txt "\\item" ,(if bullet "" "[]") " "
           ,@(if (non-empty-string? tag)
                 `("\\noindent\\hbox to 0.48\\textwidth{" ,tag "\\hfill}")
                 '())
           ,@elements)]
    [(html)
     (txexpr-ext 'li `((class "CVitem")
                   (style ,(if bullet "" "list-style-type: none;")))
             `( ,(if (non-empty-string? tag)
                     ;; put tag inside a span of fixed width, for alignment
                     (txexpr-ext 'span `((class "tag")
                                     (style ,(string-append
                                              "display: inline-block;")))
                             (list tag)) "")
                ,@elements))]))
