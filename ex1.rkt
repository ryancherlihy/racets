#lang reader "pup.rkt"

(define public-constant (const #t))
(define private-constant (mk-private public-constant))

(define a (ref public-constant)) ; create a public ref cell a with public content

(define b (ref private-constant)) ; create a public ref cell with private content

(pu-assign a private-constant) ; assign our public ref cell a new private value -> k=L, l=H therefore lift(k,l)=L

(define d (mk-private (ref public-constant))) ; create a private reference cell with a public value

(pu-assign d public-constant) ; assign our private ref cell a public value -> This should result in a partially leaked ref cell -> k=H, l=L therefore lift(k,l)=P

(define tagged-print (tag println))

(define test-if (lambda (x) (if x
                  (tagged-print #t)
                  (tagged-print #f))))






