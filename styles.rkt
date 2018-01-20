#lang racket

(require "utils.rkt")
(require pollen/setup txexpr)

(provide (all-defined-out))

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
