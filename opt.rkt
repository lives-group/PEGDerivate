#lang typed/racket
(require typed-racket-datatype)
(provide (all-defined-out))


(define-datatype (Opt a) (None) (Some [val : a]))
  
;(struct None ())
;(struct (a) Some ([val : a]))
;(define-type (Opt a) (U None (Some a)))

(: opt (All (a b) (->  (-> a b) (Opt a) b b) ) )
(define (opt f o d)
      (match o
        [(None) d]
        [(Some v) (f v)]
  ))

(: opt-fun (All (a b) (->  (-> a b) (Opt a) (Opt b)) ))
(define (opt-fun f o)
      (match o
        [(None) (None)]
        [(Some v) (Some (f v))]
      )
 )

(: opt-bind (All (a b) (->  (Opt a) (-> a (Opt b))  (Opt b)) ))
(define (opt-bind o f)
      (match o
        [(None)   (None)]
        [(Some v) (f v)]
      )
 )