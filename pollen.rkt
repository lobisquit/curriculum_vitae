#lang pollen/mode racket

(require pollen/setup pollen/decode txexpr)
(require "utils.rkt")

(module setup racket/base
  (provide (all-defined-out))
  (define poly-targets '(html ltx pdf)))

(provide (all-defined-out))

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
            ;; ignore attributes
            (elements (get-elements x))
            ;; need to filter, e.g. with (txt txt "spam")
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
