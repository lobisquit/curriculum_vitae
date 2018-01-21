#lang racket

(require pollen/setup txexpr)

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

(define (measure? x)
  (and
   (list? x)
   (= 2 (length x))
   (real? (first x))
   (string? (second x))))

(define/contract (mult-measure num measure)
  (real? measure? . -> . string?)
  (~a (* num (first measure)) (second measure)))
