#lang pollen/mode racket/base
(require racket/string)
(require racket/date txexpr pollen/setup racket/list pollen/decode)
(require racket/format)
(require pollen/tag)

(module setup racket/base
  (provide (all-defined-out))
  (define poly-targets '(html ltx pdf)))

(provide (all-defined-out))

(define (xexpr-flatten . elements)
  (define (xexpr->flat-string expr)
    (cond
      [(string? expr) expr]
      [(xexpr? expr) (string-append (xexpr->flat-string (get-elements expr)))]
      [(list? expr) (apply string-append (map xexpr->flat-string expr))]
      [else ""]))
  (xexpr->flat-string elements))

(define (url #:link [link #f] #:mail [mail #f] . elements)
  (let* (;; override default link (content) if requested
         (uri (if link link elements))
         ;; flatten uri, to remove style commands
         (uri (xexpr-flatten uri))
         ;; add mailto if requested
         (uri (string-append (if mail "mailto:" "") uri)))
    (case (current-poly-target)
      [(pdf ltx) `(txt "\\href{" ,uri  "}{" ,@elements "}")]
      [(html) (txexpr 'a `((href ,uri)) elements)])))

(define-syntax-rule (define-style latex-style html-style)
  ;; style names uses latex convention by default
  (define (latex-style . elements)
    (let ((name (symbol->string 'latex-style)))
      (case (current-poly-target)
        [(pdf ltx) (txexpr (string->symbol (~a "text" name)) empty elements)]
        [(html) (txexpr 'span '((style html-style)) elements)]))))

(define-style sc "font-variant:   small-caps;")
(define-style bf "font-weight:    bold;")
(define-style ac "text-transform: uppercase;")
(define-style sl "font-style:     oblique;")
(define-style it "font-style:     italic;")

(define (tex-C++ str)
  (case (current-poly-target)
    [(html) str]
    [(pdf ltx)
     (string-replace str "C++"
                     (string-append "C\\kern-0.2ex "
                                    "\\raise .1ex \\hbox{+\\kern-0.4ex +}"))]))

(define (root . items)
  (decode (txexpr 'body '() items)
          #:string-proc tex-C++))

(define (->LaTeX x)
  (cond
    [(txexpr? x)
     (let* ((tag (get-tag x))
           (elements (get-elements x))
           (content (apply string-append (map ->LaTeX elements))))
       (case tag
         ;; ignore "service" tags
         [(body root txt) (~a content)]
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
