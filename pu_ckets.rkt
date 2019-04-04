#lang racket

(provide (all-defined-out))

; Labels
(struct lab (name) #:transparent)

(define high (lab 'high))
(define low (lab 'low))
(define partial (lab 'partial))

; Is it either high, low, or partial?
(define (label? x) (set-member? x (set high low partial)))

; Permissive upgrade values
(struct puvalue (label raw-value) #:transparent)

; Tagged faceted closures
(struct tclo (puvalue) #:transparent)

; Tag a value with the current PC
(define (mk-labeled v)
  (puvalue (current-pc) v))

; Change a value to another label
(define (change-to value label)
  (match value
    [`(tclo (puvalue ,(? label? lab) ,raw-value))
      (tclo (puvalue label raw-value))]
    [`(puvalue ,lab ,raw-value)
      (puvalue label raw-value)]))

; Label comparison
(define (label<? l1 l2)
  (match (cons l1 l2)
    [(cons partial partial) #f]
    [(cons _ partial) #t]
    [(cons high high) #f]
    [(cons low high) #t]
    [(cons _ low) #f]))

; Label join
(define (label-join l1 l2)
  (match (cons l1 l2)
    [(cons _ partial) partial]
    [(cons partial _) partial]
    [(cons _ high) high]
    [(cons high _) high]
    [(cons _ low) low]
    [(cons low _) low]))

; The starting pc in the program
(define current-pc (make-parameter low))

#;(define (prim-to-δ op)
  (match op
    ['+ +]
    ['* *]
    ['- -]
    ['=
     (λ xs (let ([ret 1]
                 [fst (first xs)])
             (map (lambda (v)
                    (if (not (equal? v fst))
                        (set! ret 0)
                        (void))) xs)
             ret))]))
