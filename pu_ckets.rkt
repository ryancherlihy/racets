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

; Tagged permissive upgrade closures
(struct tclo (puvalue) #:transparent)

; Tag a value with the current PC
(define (mk-labeled v)
  (puvalue (current-pc) v))

; Change a value to another label
(define (change-to value label)
  (match value
    [`(tclo (puvalue ,(? label? lab) ,raw-value))
      (tclo (puvalue label raw-value))]
    [(puvalue (lab l) raw-value) ;had to remove the quoting to get this pattern matching to work for my example, might not be best-practice but seems to work for now.
      (puvalue label raw-value)]))

; Label comparison
(define (label<? l1 l2)
  (match (cons l1 l2)
    [(cons (lab 'partial) (lab 'partial)) #f]
    [(cons _ (lab 'partial)) #t]
    [(cons (lab 'high) (lab 'high)) #f]
    [(cons (lab 'low) (lab 'high)) #t]
    [(cons _ (lab 'low)) #f]))

; Label join
(define (label-join l1 l2)
  (match (cons l1 l2)
    [(cons _ (lab 'partial)) partial]
    [(cons (lab 'partial) _) partial]
    [(cons _ (lab 'high)) high]
    [(cons (lab 'high) _) high]
    [(cons _ (lab 'low)) low]
    [(cons (lab 'low) _) low]))

; Evaluate an argument passed to a closure.
(define (evaluate pc e)
  (if (puvalue? e)
      ; if the value is already tagged, join its label with the label of the evaluation context
      (change-to e (label-join pc (puvalue-label e)))
      ;else
      (if (tclo? e)
          (tclo (change-to (tclo-puvalue e) (label-join (puvalue-label (tclo-puvalue e)) (pc))))
          ; otherwise it is an untagged value and should be wrapped accordingly with respect to the evaluation context
          (puvalue pc e)
          )
      )
  )

; Evaluate a list of arguments, return evaluated list (i.e. list where each arg has been evaluated to a puvalue or tclo)
(define (list-eval pc arglist)
  (for/list ([i arglist])
    (evaluate pc i)
    )
  )

; Label lift for assignment operation
(define (label-lift l1 l2)
  (let ([lab-pair (cons l1 l2)])
    ;(fprintf (current-output-port) "Label pair being matched: ~a \n" lab-pair)
    (match lab-pair
    [(cons (lab 'low) _)
     ;(println "low _ -> Should return low: ")
     low]
    [(cons (lab 'high) (lab 'low))
     ;(println "high low -> Should return partial")
     partial]
    [(cons (lab 'high) (lab 'high))
     ;(println "high high -> Should return high")
     high]
    [(cons (lab 'high) (lab 'partial))
     ;(println "high partial -> should return partial")
     partial]
    [else
     ;(println "else case -> should return partial")
     partial]
    )))

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
