#lang typed/racket
(require typed-racket-datatype)
(require "./pegd-syntax.rkt")
(require "./pegd-derivate.rkt")
(require "./opt.rkt")
(require "./env.rkt")
(require "./boolsolver.rkt")
(require "./ftable.rkt")

(provide (all-defined-out))



(define (ft-from-grm [v : (ListEnv DPE)]) : FTable
    (let* ([names : (Listof String) (map (lambda ([k : (Pair String DPE)]) (car k)) v)]
           [tb : FTable (mk-empty-ftable names)]
           [nll : (Listof (Pair String Boolean))
                 (dpe-env-null v)])
           (foldr (lambda ([e : (Pair String Boolean)] [f : FTable]) (ft-set-nullable f (car e) (cdr e))) tb nll)
     )
  )


(define (first1  [Σ : (Listof Char)] [ l : FTable] [p : DPE]  ) :  (Listof Char)
     (match p
        [(p∅)           null]
        [(p?)            Σ]
        [(pϵ)            null]
        [(pSym c)        (list c)]
        [(pVar s)        (ft-get l s)]
        [(pCat p1 p2)    (cond [(quick-dpe-null? l p1) (set-union (first1 Σ l p1) (first1 Σ l p2))]
                               [else             (first1 Σ l p1) ]) ]
        [(pAlt p1 p2)    (set-union (first1 Σ l p1) (first1 Σ l p2))]
        [(pKle p)        (first1 Σ l p)]
        [(pNot p)        (first1 Σ l p)] 
        [_               null]
     )
  )


(define (iterate-first-table  [Σ : (Listof Char)] [ l : FTable]  [v : (ListEnv DPE) ] ) :  FTable
  (let ([tab : FTable (foldr (lambda ([x : (Pair String DPE)] [t : FTable])  (ft-ins-all t (car x) (first1 Σ t (cdr x)) ) ) l v) ])
       (cond [(ft-changed? tab) (iterate-first-table Σ (ft-rst tab) v)]
             [else tab])
  )
)


(define (dpeg-first  [ d : DPEG] ) :  FTable
     (iterate-first-table (alphabet-from-grammar d) (ft-from-grm (DPEG-dv d) ) (DPEG-dv d))
)

(define (quick-dpe-null?  [ft : FTable ] [e : DPE]) : Boolean
       (match e
        [(p∅)           #f]
        [(p?)            #f]
        [(pϵ)            #t]
        [(pSym c)        #f]
        [(pVar s)        (ft-is-nullable? ft s)]
        [(pCat p1 p2)    (let ([r : Boolean (quick-dpe-null? ft p1)] )
                              (cond [r (quick-dpe-null? ft p2)] [else r]))]
        [(pAlt p1 p2)     (let ([r : Boolean (quick-dpe-null? ft p1)] )
                               (cond [(not r) (quick-dpe-null? ft p2)] [else r]))] 
        [(pKle p)        #t]
        [(pNot p)        (not (quick-dpe-null? ft p))] 
        [_        (error "tried to determine if a peding operation is null !")]
     )
  )

(define (cat-sol [x : Char] [l : (Opt (Listof Char))]) : (Opt (Listof Char))
     (match l
       [(None)    (Some (list x))]
       [(Some xs) (Some (cons x xs))])
  )

(define  (one-of [xs : (Listof Char)]): Char
    (cond
      [(null? xs) #\000]
      [else       (car xs)])
  )

(define (peg-syn [n : Natural] [Σ : (Listof Char)] [ft : FTable ] [g : DPEG] ) : (Opt (Listof Char))
     (let* ([fs : (Listof Char) (first1 Σ ft (DPEG-ds g))]
            [acc : Boolean (quick-dpe-null? ft (DPEG-ds g))]
            [ch : Char (one-of fs)])
          (cond
            [(and (or (<= n 0) (null? fs)) acc)       (Some null)]
            [(and (or (<= n 0) (null? fs)) (not acc)) (None)]
            [acc                                      (cat-sol (one-of fs)
                                                               (peg-syn (- n 1) Σ ft (derivate-grammar ch g)) )]
            [else (opt-fun (lambda ([l : (Listof Char)]) (cons ch l))
                           (peg-syn (- n 1) Σ ft (derivate-grammar ch g)) ) ])
       )
  )

(define (peg-input-syn [g : DPEG] ) : (Opt (Listof Char))
   (peg-syn 10 (alphabet-from-grammar g) (dpeg-first g) g)
  )

