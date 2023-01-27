#lang typed/racket
(require typed-racket-datatype)
(require "./opt.rkt")

(provide (all-defined-out))

(define-type (ListEnv a)  (Listof (Pair String a)) )

(: lkp (All (a) (-> (ListEnv a) String (Opt a)) ))
(define (lkp v s)
      (match v
        ['() (None)]
        [(cons (cons n p) xs) (cond
                                [(string=?  n s) (Some p)]
                                [else            (lkp xs s)])]))

(: env-ins (All (a) (-> (ListEnv a) String a (ListEnv a)) ))
(define (env-ins env k v)
  (cons (cons k v) env))

(: env-ins-unique (All (a) (-> (ListEnv a) String a (ListEnv a)) ))
(define (env-ins-unique env k v)
  (match env
    ['() null]
    [(cons (cons k1 v1) xs) (cond [(string=? k k1) (cons (cons k1 v) xs)]
                                  [else (cons (cons k1 v1) (env-ins-unique xs k v))])]
  )
)

(: env-set (All (a) (-> (ListEnv a) String a (ListEnv a)) ))
(define (env-set env k v)
  (cond
    [(null? env) (list (cons k v))]
    [else (cond
             [(string=? k (car (car env))) (cons (cons k v) (cdr env)) ]
             [else (cons (car env) (env-set (cdr env) k v) )]) ] )

  )

(: env-append (All (a) (-> (ListEnv a) (ListEnv a) (ListEnv a)) ))
(define (env-append env1 env2)
  (append env1 env2))

(: env-vals (All (a) (-> (ListEnv a) (Listof a)) ))
(define (env-vals env1)
  (map (lambda ([p : (Pair String a)]) (cdr p)) env1))

(: env-pred-keys (All (a) (-> (-> a Boolean) (ListEnv a) (Listof String)) ))
(define (env-pred-keys f env1)
  (filter-map (lambda ([p : (Pair String a)]) (and (f (cdr p)) (car p)) ) env1) )

(: env-map-vals (All (a b) (-> (-> a b) (ListEnv a) (Listof b)) ))
(define (env-map-vals f env1)
  (map (lambda ([p : (Pair String a)]) (f (cdr p)) ) env1) )

(: env-map (All (a b) (-> (-> a b) (ListEnv a) (ListEnv b)) ))
(define (env-map f env1)
  (map (lambda ([p : (Pair String a)]) (cons (car p) (f (cdr p))) ) env1) )

(: env-trasform (All (a b) (-> (-> String a b) (ListEnv a) (Listof b)) ))
(define (env-trasform f env1)
  (map (lambda ([p : (Pair String a)]) (f (car p) (cdr p)) ) env1) )