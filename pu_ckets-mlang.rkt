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
  [pu-module-begin #%module-begin]
  [pu-app #%app]
  [pu-lambda #%plain-lambda]
  [pu-lambda lambda]
  [pu-if if]
  [pu-and and]
  [pu-or or]
  )

 ; Create a constant
 const

 ; Create a tagged function <-placeholder, not sure we actually want this function long-term
 tag

 ; Create a labeled value
 label

 ; Create a reference cell
 ref

 ; dereference a reference cell
 deref

 ; mutate a reference cell
 ref-set!
 

 ; privatize a puvalue
 mk-private

 ; mutate a reference cell
 pu-assign
)

; Constants are simply marked with the current PC
(define-syntax (const stx)
  (syntax-parse stx
    [(_ c) #`(mk-labeled c)]))

(define-syntax (tag stx)
  (syntax-parse stx
    [(_ clo) #`(mk-tagged clo)]))

; Lambdas are rewritten into tagged closures so we can implement
; `racets` closures from primitives.
(define-syntax (pu-lambda stx)
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
(define-syntax (pu-module-begin stx)
  (syntax-parse stx
      [(#%module-begin body ...)
       #`(#%plain-module-begin
          body ...)]))

; If
(define-syntax (pu-if stx)
  (syntax-parse stx
    [(_ guard et ef)
         ; evaluate the guard <- make sure it is tagged and has a raw boolean value
         #'(let* ([evaled-guard (evaluate (current-pc) guard)][guard-raw (puvalue-raw-value guard)][guard-lab (puvalue-label guard)])
         ; make sure the guard isn't partially leaked (i.e. not k=P)
           (if (not (equal? guard-lab partial))
               (if (boolean? guard-raw)
                    ; parameterize the current pc to the label of the guard (k).
                    (parameterize ([current-pc guard-lab])
                      (fprintf (current-output-port) "If->current-pc: ~a \n" (current-pc))
                      (if guard-raw
                          ; if the guard is true evaluate the then branch with respect to k.
                          (evaluate (current-pc) et)
                          ; if the guard is false evaluate the else branch with respect to k.
                          (evaluate (current-pc) ef)
                          )
                      )
                   (error "Guard condition does not evaluate to a boolean")
                   )
               (error "Guard is partially leaked, cannot procede."))
             (fprintf (current-output-port) "If->current-pc: ~a \n" (current-pc))
         )]))

; ref
(define-syntax (ref stx)
  (syntax-parse stx
    [(_ e)
     #`(let ([var e]) ; let-bind e to evaluate
         (if (puvalue? var)
             (mk-labeled (box var))
             (error "Expected a tagged value")))])) ; create a new puvalue of the box tagged with the pc


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
;(define-syntax (deref stx)
;  (syntax-parse stx
;    [(_ e)
;     #`(let ([addr e])
;         (if (puvalue? addr) ;check that the supplied value is tagged with a security label
;             (if (box? (puvalue-raw-value addr)) ;check that the supplied value is a reference cell
;                 (change-to (unbox (puvalue-raw-value addr)) (label-join
;                                                              (puvalue-label addr)
;                                                              (puvalue-label (unbox (puvalue-raw-value addr))))) ; return the value pointed to by the reference cell, with a new label that is a join of the value and the address which held it
;                 (error "Derefence target is not a reference cell")
;                 )
;             (error "Dereference target not tagged with a security label")
;          )
;         )]))
(define-syntax (deref stx)
  (syntax-parse stx
    [(_ e)
     #`(let ([addr e])
         (if (puvalue? addr) ;check that the supplied value is tagged with a security label
             (if (box? (puvalue-raw-value addr)) ;check that the supplied value is a reference cell
                 (let* ([unboxed (unbox (puvalue-raw-value addr))]
                        [new-lab (label-join (puvalue-label addr) (puvalue-label unboxed))]
                        )
                   (set-puvalue-label! unboxed new-lab)
                   unboxed)
                 (error "Derefence target is not a reference cell")
                 )
             (error "Dereference target not tagged with a security label")
          )
         )]))


; privatization operation changes the security label of a puvalue to high
(define-syntax (mk-private stx)
  (syntax-parse stx
  [(_ e)
   #'(let ([val e])
       (if (puvalue? val)
           (set-puvalue-label! val high)
           (error "Privatization target is not a tagged with a security label")))]))


; assign-permissive - function deals with assignment to an already occupied reference cell
(define-syntax (pu-assign stx)
  (syntax-parse stx
    [(_ e1 e2)
     #'(let ([val1 (evaluate (current-pc) e1)]
             [val2 (evaluate (current-pc) e2)])
             (if (puvalue? val1)
                 (if (puvalue? val2)
                      (if (box? (puvalue-raw-value val1))
                          (let* ([curr (unbox (puvalue-raw-value val1))]
                                [curr-label (puvalue-label curr)]
                                [new-raw (puvalue-raw-value val2)]
                                [new-label (puvalue-label val2)]
                                [addr-label (puvalue-label val1)])
                            (if (not (equal? addr-label partial))
                                (let ([label-m (label-lift addr-label curr-label)])
                                  ;(fprintf (current-output-port) "val1: ~a \n" val1)
                                  ;(fprintf (current-output-port) "val2: ~a \n" val2)
                                  ;(fprintf (current-output-port) "curr: ~a \n" curr)
                                  ;(fprintf (current-output-port) "curr-label: ~a \n" curr-label)
                                  ;(fprintf (current-output-port) "new-raw: ~a \n" new-raw)
                                  ;(fprintf (current-output-port) "new-label: ~a \n" new-label)
                                  ;(fprintf (current-output-port) "new-label: ~a \n" addr-label)
                                  ;(fprintf (current-output-port) "label-m is the product of lifting the address label and the data label: \n ~a = (label-lift ~s ~v) \n" label-m addr-label curr-label)
                                  ;(puvalue label-m (box (puvalue (label-join label-m new-label) new-raw))))
                                  (set-box! (puvalue-raw-value val1) (puvalue (label-join label-m new-label) new-raw))
                                  (set-puvalue-label! val1 label-m))
                                (error "Address is partially leaked, cannot procede.")
                                )
                            )
                      (error "Assignment target is not a reference cell"))
                 (error "Value to be assigned is not tagged with a security label"))
             (error "Assignment target is not tagged with a security label")
             ))
     ]
    )
  )

; Faceted application
(define-syntax (pu-app stx)
  (syntax-parse stx
    [(_ f arg0 ...)
    #:with ooo (quote-syntax ...)
      #`(let ([clo f])
          (match clo
            [(tclo (puvalue lbl underlying-clo))
             ; It evaluates to a closure
             ; We want to evaluate each of arg0 ... w/ *current* pc
             ; before switching to the new lbl to actually perform the
             ; application.
             (let* ([arglist (list* arg0 ...)] ; Just put all the arguments into a let bound list*
                    [evald-args (list-eval (current-pc) arglist)]) 
               ;(fprintf (current-output-port) "Matched a closure: ~a \n" clo)
               ;(fprintf (current-output-port) "Label: ~a \n" lbl)
               ;(fprintf (current-output-port) "Underlying closure: ~a \n" underlying-clo)
               ;(fprintf (current-output-port) "Argument list: ~a \n" arglist)
               ;(fprintf (current-output-port) "current-pc: ~a \n" (current-pc))
               ;(fprintf (current-output-port) "Evaluated argument list: ~a \n" evald-args)
               (parameterize ([current-pc lbl])
                 (fprintf (current-output-port) "Function Application->current-pc: ~a \n" (current-pc))
                 (if (equal? (current-pc) partial)
                     (error "Function cannot be applied as this function is partially leaked.")
                     (evaluate (current-pc) (apply underlying-clo evald-args))
                 )
               )
               ;(fprintf (current-output-port) "Function Application->current-pc: ~a \n" (current-pc))
               )
             ]
            ;[(puvalue lab raw_val) ;if for some reason we get a puvalue rather then a tclo, just return it. <-- I actually think this was because I had written (puvalue) in one of my functions, so this clause might not actually be necessary.
             ;(puvalue lab raw_val)
             ;]
            [println
             (println arg0 ...)]
            [fprintf
             (fprintf arg0 ...)]
            )
          )
      ]
    )
  )
;
; And/or
;
(define-syntax (pu-and stx)
  (syntax-parse stx
    [(_) #`#t]
    [(_ e0 es ...)
     #`(pu-if e0 (pu-and es ...) #f)]))

(define-syntax (pu-or stx)
  (syntax-parse stx
    [(_) #`#f]
    [(_ e0 es ...)
     #`(pu-if e0 #t (pu-or es ...))]))

; Not sure what to do with continuations, we will have to handle other
; continuation-based stuff, too, eventually.
(define-syntax (fac-continuation-mark stx)
  (wrong-syntax stx "continuation marks are currently unsupported in racets"))
