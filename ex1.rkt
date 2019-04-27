#lang reader "pup.rkt"

;(define public-constant (const #t))
;(define private-constant (mk-private public-constant))

;(define a (ref public-constant)) ; create a public ref cell a with public content

;(define b (ref private-constant)) ; create a public ref cell with private content

;(pu-assign a private-constant) ; assign our public ref cell a new private value -> k=L, l=H therefore lift(k,l)=L

;(define d (mk-private (ref public-constant))) ; create a private reference cell with a public value

;(pu-assign d public-constant) ; assign our private ref cell a public value -> This should result in a partially leaked ref cell -> k=H, l=L therefore lift(k,l)=P

(define tagged-print (tag println))

(define test-if (lambda (x) (if x
                  (tagged-print #t)
                  (tagged-print #f))))

(define y (ref (const #t)))

(define z (ref (const #t)))

(define w (ref y))

(define h (lambda (var)
             (let ([x var])
               (fprintf (current-output-port) "Calling h(x) where x is ~a. \n" x)
               (fprintf (current-output-port) "y is ~a. \n" y)
               (fprintf (current-output-port) "z is ~a. \n" z)
               (fprintf (current-output-port) "w is ~a. \n" w)
               (if (deref x)
                   (let ([branched-on x])
                     (println "x evaluated to #t. Since x is private, pc --> H for the duration of the \"if\" clause.")
                     (pu-assign w z)
                     (println "Assigned w to z within a private context, therefore w should contain z with a partially leaked tag.")
                     (fprintf (current-output-port) "z is ~a. \n" z)
                     (fprintf (current-output-port) "w is ~a. \n" w)
                     )
                   (println "x evaluated to #f.")
                   )
               (println "Assigning the low security value #f to the reference cell pointed to by w")
               (pu-assign (deref w) (const #f))
               (println "Derefencing y")
               (deref y)
               )
             )
  )

(define h-priv (lambda (var)
             (let ([x var])
               (fprintf (current-output-port) "Calling h(x) where x is ~a. \n" x)
               (fprintf (current-output-port) "y is ~a. \n" y)
               (fprintf (current-output-port) "z is ~a. \n" z)
               (fprintf (current-output-port) "w is ~a. \n" w)
               (if (deref x)
                   (let ([branched-on x])
                     (println "x evaluated to #t. Since x is private, pc --> H for the duration of the \"if\" clause.")
                     (pu-assign w z)
                     (println "Assigned w to z within a private context, therefore w should contain z with a partially leaked tag.")
                     (fprintf (current-output-port) "z is ~a. \n" z)
                     (fprintf (current-output-port) "w is ~a. \n" w)
                     )
                   (println "x evaluated to #f.")
                   )
               (println "Assigning the low security value #f to the reference cell pointed to by w")
               (mk-private (deref w))
               (pu-assign (deref w) (const #f))
               (println "Derefencing y")
               (deref y)
               )
             )
  )


; For all tests: x is a private reference cell

; Test A -> function h(x) with x being false
(define a (const #f))
(mk-private a)
;(define h-fh (h (ref a)))

; Test B -> function h(x) with x being true
(define b (const #t))
(mk-private b)
;(define h-th (h (ref b)))

; Test C -> function h-priv(x) with x being false
(define c (const #f))
(mk-private c)
;(define h-pfh (h-priv (ref a)))

; Test D -> function h-priv(x) with x being true
(define d (const #t))
(mk-private d)
;(define h-th (h-priv (ref b)))




