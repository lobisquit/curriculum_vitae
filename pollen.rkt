#lang pollen/mode racket/base
(require racket/string)
(require racket/date txexpr pollen/setup racket/list pollen/decode)
(require racket/format)
(require pollen/tag)
(provide (all-defined-out))

(module setup racket/base
  (provide (all-defined-out))
  (define poly-targets '(html ltx pdf)))

(define (sl . elements)
  (case (current-poly-target)
    [(pdf ltx) (txexpr 'textsl empty elements)]
    [(html)    (txexpr 'i empty elements)     ]))

(define (url #:link [link #f] #:mail [mail #f] . elements)
  (let* (;; flatten content, to extract url (if needed)
         ;; NOTE works only with HTML, since LaTeX is already
         ;; converted to string and cannot be flattened (yet)

         ;; (asd (display (~a "link: " link "\n")))
         (asd (display (~a "elements: " (flatten elements) "\n")))
         (uri (apply string-append (filter string? (flatten elements))))

         ;; override default link if requested
         (uri (if link link uri))

         ;; add mailto if requested
         (uri (string-append (if mail "mailto:" "") uri))
         (asd (display (~a "uri: " uri "\n")))
         )

    (case (current-poly-target)
      [(pdf ltx html) (let ((expr (txexpr 'txt empty `("\\href{" ,uri  "}{" ,@elements "}"))))
                   (begin
                     (display (~a "------->> " expr))
                     expr)
                   )]
      [(html) (txexpr 'a `((href ,uri)) elements)])))

(define (tex-C++ str)
  (case (current-poly-target)
    [(pdf ltx) (string-replace str "C++"
                               (string-append
                                "C\\kern-0.2ex "
                                "\\raise .1ex \\hbox{+\\kern-0.4ex +}"))]
    [(html) str]))

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

(define (root . items)
  (decode (txexpr 'body '() items)
          #:string-proc tex-C++))

(define (make-latex-source doc)
  ◊string-append{
                 \documentclass[a4paper,12pt]{letter}
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
                 ◊(apply string-append (filter string? (flatten doc)))
                 \end{document}
                 })
