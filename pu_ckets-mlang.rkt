#lang racket

(require "pu_ckets.rkt" (for-syntax "pu_ckets.rkt"))
(require (for-syntax racket/syntax))
(require (for-syntax racket/stxparam))
(require (for-syntax racket/set))
(require (for-syntax racket/base syntax/parse))
(require racket/stxparam)

(provide 
 ; Buit on Racket
 (except-out
  (all-from-out racket)
  with-continuation-mark
  #%module-begin
  #%plain-lambda
  lambda
  if
  and
  or
  #%app)
 ; Rename out our core forms
 (rename-out
  [fac-module-begin #%module-begin]
  [fac-app #%app]
  [fac-lambda #%plain-lambda]
  [fac-lambda lambda]
  [fac-if if]
  [fac-and and]
  [fac-or or]
  )

 ; Create a constant
 const

 ; Create a labeled value
 label

 ; Create a reference cell
 ref

 ; dereference a reference cell
 deref

 ; mutate a reference cell
 ref-set!
 )

; Constants are simply marked with the current PC
(define-syntax (const stx)
  (syntax-parse stx
    [(_ c) #`(mk-labeled c)]))

; Lambdas are rewritten into tagged closures so we can implement
; `racets` closures from primitives.
(define-syntax (fac-lambda stx)
  (syntax-parse stx
    [(_ xs expr)
    #`(tclo (mk-labeled (lambda xs expr)))]))

; Create a labeled value
(define-syntax (label stx)
  (syntax-parse stx
    [(_ e)
     #`(change-to e high)]))

; Module begin (currently this does nothing, eventually... integrate
; Matt's code..?)
(define-syntax (fac-module-begin stx)
  (syntax-parse stx
      [(#%module-begin body ...)
       #`(#%plain-module-begin
          body ...)]))

; If
(define-syntax (fac-if stx)
  (syntax-parse stx
    [(_ guard et ef) #`(void)]))

; ref
(define-syntax (ref stx)
  (syntax-parse stx
    [(_ vr)
     #`(let ([var vr]) ; let-bind vr to evaluate
         (if (facet? var)
             ; if var is a facet,
             (box (construct-facet-optimized (set->list (current-pc)) var (lfail)))
             ; else
             (box var)))]))


; ref-set!
(define-syntax (ref-set! stx)
  (syntax-parse stx
    [(_ var e)
     #`(let ([value e])
         (let setf ([var var]
                    [pc (current-pc)])
           (if (box? var)
               ; if var is a facet value
               (set-box! var (construct-facet-optimized (set->list (current-pc)) value (unbox var)))
               ; else
               (mkfacet
                (facet-labelname (unbox var))
                (setf
                 (facet-left (unbox var))
                 (set-add pc (pos (facet-labelname var))))
                (setf
                 (facet-right (unbox var))
                 (set-add pc (neg (facet-labelname var))))))))]))

; deref
(define-syntax (deref stx)
  (syntax-parse stx
    [(_ vr)
     #`(let dereff ([var (unbox vr)])
         (if (facet? var)
             ; if var is a facet value
             (cond
               [(set-member? (current-pc) (pos (facet-labelname var)))
                (dereff (facet-left var))]
               [(set-member? (current-pc) (neg (facet-labelname var)))
                (dereff (facet-right var))]
               [else
                (mkfacet (facet-labelname var) (dereff (facet-left var)) (dereff (facet-right var)))])
             ; else
             var))]))

; Faceted application
(define-syntax (fac-app stx)
  (syntax-parse stx
    [(_ f . args)
     #`(let applyf ([func f])
         (cond
           [(facet? func)
            (let* ([left
                    (parameterize ([current-pc
                                    (set-add (current-pc)
                                             (pos (facet-labelname func)))])
                      (applyf (facet-left func)))]
                   [right
                    (parameterize ([current-pc
                                    (set-add (current-pc)
                                             (neg (facet-labelname func)))])
                      (applyf (facet-right func)))])
              (construct-facet-optimized
               (list (pos (facet-labelname func)))
               left
               right))]
           ; An fclo coming from Racets
           [(fclo? func) ((fclo-clo func) . args)]
           ; Not an fclo. Must be a builtin, etc..
           [else ((facet-fmap* func) . args)]))]))

;
; And/or
;
(define-syntax (fac-and stx)
  (syntax-parse stx
    [(_) #`#t]
    [(_ e0 es ...)
     #`(fac-if e0 (fac-and es ...) #f)]))

(define-syntax (fac-or stx)
  (syntax-parse stx
    [(_) #`#f]
    [(_ e0 es ...)
     #`(fac-if e0 #t (fac-or es ...))]))

; Not sure what to do with continuations, we will have to handle other
; continuation-based stuff, too, eventually.
(define-syntax (fac-continuation-mark stx)
  (wrong-syntax stx "continuation marks are currently unsupported in racets"))
