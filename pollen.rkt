#lang pollen/mode racket/base
(require racket/string)
(require racket/date txexpr pollen/setup racket/list pollen/decode)
(require racket/format)
(require pollen/tag)
(provide (all-defined-out))

(module setup racket/base
  (provide (all-defined-out))
  (define poly-targets '(ltx pdf)))

(define (sl . elements)
  `(txt "\\textsl{" ,@elements "}"))

(define (url #:link [link #f] #:mail [mail #f] . elems)
  `(txt "\\href{" ,(if mail "mailto:" "") ,@(if link (list link) elems)  "}{" ,@elems "}"))

(define (tex-C++ str)
  (string-replace str "C++" "C\\kern-0.2ex \\raise .15ex \\hbox{+\\kern-0.4ex +}"))

(define (CVheader . elements)
  `(txt "\\begin{center}\n" ,@elements "\n\\end{center}"))

(define (CVname . elements)
  `(txt "{\\Huge " ,(apply string-upcase elements) "}\n"))

(define (CVaddress . elements)
  `(txt "{\\large \\textsc{" ,@elements "}}\n"))

(define (CVphone . elements)
  `(txt "{\\normalsize " ,@elements "}"))

(define (CVemail . elements)
  `(txt "{\\normalsize \\href{mailto:" ,@elements "}{" ,@elements "}}"))

(define (CVsection #:title title . elems)
    `(txt "\\vspace{10mm} \\hrule \\vspace{ -2mm}"
      "\n{\\scshape \\bfseries \\small " ,title "}"
      "\n\n ",@elems))

(define (CVobject #:period [period ""] . elems)
    `(txt "{\\large " ,@elems " \\hfill " ,period "}\n"))

(define (CVitems . elems)
  `(txt "\\begin{itemize}["
    ;; no big margin
    "leftmargin=*,"
    ;; tiny bullet
    "label=\\raisebox{0.25ex}{\\tiny$\\bullet$}]\n"
    ,@elems
    "\n\\end{itemize}"))

(define (CVitem
         #:bullet [bullet #t]
         #:tag [tag #f]
         #:tagsize [tagsize "7cm"]
         . elems)
  ;; conditionally set pieces of item
  `(txt "\\item" ,(if bullet "" "[]") " "
        ,@(if tag `("\\noindent\\hbox to" ,tagsize "{" ,tag "\\hfill}") '())
        ,@elems))

(define (root . items)
  (decode (txexpr 'root '() items)
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
