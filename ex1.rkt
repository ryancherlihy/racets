#lang reader "pup.rkt"

(define y (const #t))
(define z (const #f))

(define a (ref y))

(define b (deref a))

(define c (mk-private b)) ; private puvalue

(define d (pu-assign a c)) ; public reference cell with private content

(define e (mk-private a)) ; private reference cell with public content

(define f (pu-assign e z))

(define g (deref f))