#lang typed/racket
(require typed-racket-datatype)
(require "./pegd-syntax.rkt")
(require "./opt.rkt")
(require "./env.rkt")
(require "./boolsolver.rkt")
(require "./ftable.rkt")

(provide (all-defined-out))



(define (dpe-exp-null  [e : DPE ] ) : CExp
       (match e
        [(p∅)           mkFalse]
        [(p?)            mkFalse]
        [(pϵ)            mkTrue]
        [(pSym _)        mkFalse]
        [(pVar s)        (mkVar s)]
        [(pCat p1 p2)    (mkAnd (dpe-exp-null p1) (dpe-exp-null p2))]
        [(pAlt p1 p2)    (mkOr (dpe-exp-null p1) (dpe-exp-null p2))] ;(tor (dpe-null? v p1) (dpe-null? v p2))]
        [(pKle _)        mkTrue]
        [(pNot p)        (mkNot (dpe-exp-null p))] ; This needs not to be lazy here !
        [_        (error "tried to determine if a peding operation is null !")]
     )
  )

(define (dpe-env-null  [v : (ListEnv DPE) ] ) : (ListEnv Boolean)
      (env-map isTrue? (solve-env (env-map (lambda ([x : DPE])  (dpe-exp-null x)) v)))
  )

;
;
(define (δ [a : Char] [v : (ListEnv DPE)] [p : DPE]) :  DPE
     (match p
        [(p∅)           (p∅)]
        [(p?)            (p∅)]
        [(pϵ)            (pϵ)]
        [(pSym _)        (p∅)]
        [(pVar s)        (δP a (dlkup v s))]
        ;[(δP c (pVar s)) (δ c v (dlkup v s))]
        [(δP c (pKle p)) (δ c v (pKle p))]
        [(pCat p1 p2)    (dcat (δ a v p1) (δ a v p2))]
        [(pAlt p1 p2)    (dalt (δ a v p1) (dcat (δ a v (dnot p1)) (δ a v p2)) )]
        [(pKle p)        (dalt (dcat (δ a v p) (δP a (pKle p)))
                               (dcat (δ a v (dnot p)) (δP a  (pKle p) ) )) ]
        [(pNot p)        (dnot  (DP a (dcat p (pKle (p?)) )) )] ; This needs not to be lazy here ! 
     )
  )


(define (d [a : Char] [v : (ListEnv DPE)] [p : DPE]) :  DPE
     (match p
        [(p∅)           (p∅)]
        [(p?)            (pϵ)]
        [(pϵ)            (p∅)]
        [(pSym c)        (cond [(eq? c a) (pϵ)] [else (p∅)] )]
        [(pVar s)        (DP a (dlkup v s))]
        [(δP c (pVar s)) (d a v (δ c v (dlkup v s)))]
        [(δP c (pKle p)) (d a v (δ c v (pKle p)))]
        ;[(DP c (pVar s)) (d a v (d c v (dlkup v s)))]
        ;[(DP c (pKle p)) (d a v (d c v (pKle p)))]
        [(pCat p1 p2)    (dalt  (dcat (d a v p1) p2) (dcat (δ a v p1) (d a v p2) ))]
        [(pAlt p1 p2)    (dalt (d a v p1) (d a v p2) )]
        [(pKle p)        (dalt (dcat (d a v p)  (pKle p) )
                               (dcat (δ a v  p) (DP a (pKle p) ) )) ]
        [(pNot p)        (p∅)] ; This needs not to be lazy here !
        [(DP c p)        (d a v (d c v p))]
     )
  )

; ed stands for expand derivate. It recursively traverses the PEG
; and only expands all pendind derivate and delta computations 
(define (ed [v : (ListEnv DPE)] [p : DPE]) :  DPE
     (match p
        [(pCat p1 p2)    (dcat (ed v p1) (ed v p2))]
        [(pAlt p1 p2)    (dalt (ed v p1) (ed v p2))]
        [(pKle e)        (pKle (ed v e))]
        [(pNot e)        (dnot (ed v e))]
        [(DP c (pVar s)) (d c v (dlkup v s))]
        [(δP c (pVar s)) (δ c v (dlkup v s))]
        [(DP c e)        (d c v e)]
        [(δP c e)        (δ c v e)]
        [e  e]
     )
  )

; Lazyly Expands unevaluated derivates.
; Pendind computation are not completly exapand and any
; aditional computation required is oly marked as pending
; computation
(define (ed1 [v : (ListEnv DPE)] [p : DPE]) :  DPE
     (match p
        [(pCat p1 p2)    (dcat (ed1 v p1) (ed1 v p2))]
        [(pAlt p1 p2)    (dalt (ed1 v p1) (ed1 v p2))]
        [(pKle e)        (pKle (ed1 v e))]
        [(pNot e)        (dnot (ed1 v e))]
        [(DP c (pCat p1 p2))    (dalt (dcat (DP c p1) p2)  (dcat (δP c p1) (DP c p2) ))]
        [(DP c (pAlt p1 p2))    (dalt (DP c p1)  (DP c p2)  )]
        [(DP c (pKle e))        (dalt (dcat (DP c e) (pKle e))  (dcat (δP c e) (DP c (pKle e)) ))]
        [(DP c (pNot e))        (p∅)]
        [(DP c (pVar s)) (DP c (dlkup v s))]
        [(DP c (pSym x)) (cond [(char=? c x) (pϵ)] [else (p∅)]) ]
        [(DP c (p?))     (pϵ)]
        [(DP c (pϵ))     (p∅)]
        [(DP c (p∅))    (p∅)]
        [(δP c (pVar s)) (δP c (dlkup v s))]
        [(DP c e)        (d c v e)]
        [(δP c e)        (δ c v e)]
        [e  e]
     )
  )



(define (iterate-expand [v : (ListEnv DPE)] [e : DPE] ) : DPE
     (begin
        ;(println (dpe-pprint 0 e))
        (cond
            [(dpe-pending? e) (iterate-expand v (ed v e))]
            [else e]))
)

(define (step-derivate  [fuel : Natural]  [v : (ListEnv DPE) ] [e : DPE] ) : (Listof DPE)
    (cond
      [(or (<= fuel 0) (not (dpe-pending? e))) (list e)]
      [else (cons e (step-derivate (- fuel 1) v (ed v e)) )]
  )
)

(define (step-expand  [fuel : Natural] [v : (ListEnv DPE) ] [e : DPE] )
   (for ([x  (step-derivate fuel v e) ])
        (pprint-dpe x)
        (displayln " ")
   )
)

(define (step-der  [fuel : Natural] [c : Char] [e : DPEG ] )
   (for ([x  (step-derivate fuel (DPEG-dv e) (d c (DPEG-dv e) (DPEG-ds e))) ])
        (pprint-dpe x)
        (displayln " ")
   )
)

; Derivate the PEG in relation to character c. 
;
;
(define (derivate [c : Char] [g : DPEG] ) : DPE
   (iterate-expand (DPEG-dv g) (d c (DPEG-dv g) (DPEG-ds g)))
  )

(define (derivate-grammar [c : Char] [g : DPEG] ) : DPEG
   (DPEG (DPEG-dv g)
         (iterate-expand (DPEG-dv g) (d c (DPEG-dv g) (DPEG-ds g)))
  )
)
(define (derivateWith [c : Char] [g : (ListEnv DPE)] [e : DPE] ) : DPE
   (iterate-expand g (d c g e))
  )



