#lang typed/racket
(require typed-racket-datatype)


(provide (all-defined-out))

(define-datatype Trilog (true) (false) (undef))

(define (tand [l : Trilog] [r : Trilog]) : Trilog
  (match (cons l r)
     [(cons (false) _) (false)]
     [(cons _ (false)) (false)]
     [(cons (true) y) y]
     [(cons x (true)) x] 
     [(cons (undef) (undef)) (undef)]
    )
 )


(define (tor [l : Trilog] [r : Trilog]) : Trilog
  (match (cons l r)
     [(cons (true) _) (true)]
     [(cons _ (true)) (true)]
     [(cons (false) y) y]
     [(cons x (false)) x]
     [(cons (undef) (undef)) (undef)]
    )
 )

(define (tnot [l : Trilog]) : Trilog
  (match  l
     [(true)  (false)]
     [(false) (true)]
     [(undef) (undef)]
    )
 )
