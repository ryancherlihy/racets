#lang reader "pup.rkt"

(define y (const #t))
(define z (const #t))

(define a (ref y))

(define b (deref a))

(define c (mk-private (b)))