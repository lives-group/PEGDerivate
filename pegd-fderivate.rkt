#lang typed/racket
(require typed-racket-datatype)
(require "./pegd-syntax.rkt")
(require "./opt.rkt")
(require "./env.rkt")
(require "./ftable.rkt")

(provide (all-defined-out))



(define (fδ [f : FTable] [p : DPE]) :  DPE
     (cond
       [(quick-dpe-null? f p) (pϵ)]
       [else (p∅)]
     )
 )

(define (fd [a : Char] [f : FTable] [v : (ListEnv DPE)] [p : DPE]) :  DPE
     (match p
        [(p∅)           (p∅)]
        [(p?)            (pϵ)]
        [(pϵ)            (p∅)]
        [(pSym c)        (cond [(eq? c a) (pϵ)] [else (p∅)] )]
        [(pVar s)        (DP a (dlkup v s))]
        [(δP c (pVar s)) (fd a f v (fδ f (dlkup v s)))]
        [(δP c (pKle p)) (fd a f v (fδ f (pKle p)))]
        [(pCat p1 p2)    (dalt  (dcat (fd a f v p1) p2) (dcat (fδ f p1) (fd a f v p2) ))]
        [(pAlt p1 p2)    (dalt (fd a f v p1) (fd a f v p2) )]
        [(pKle p)        (dalt (dcat (fd a f v p)  (pKle p) )
                               (dcat (fδ f p) (DP a (pKle p) ) )) ]
        ;[(pNot p)        (dnot (d a v p))] 
        [(pNot p)        (p∅)] 
        [(DP c p)        (fd a f v (fd c f v p))]
     )
  )


(define (fed [f : FTable] [v : (ListEnv DPE)] [p : DPE]) :  DPE
     (match p
        [(pCat p1 p2)    (dcat (fed f v p1) (fed f v p2))]
        [(pAlt p1 p2)    (dalt (fed f v p1) (fed f v p2))]
        [(pKle e)        (pKle (fed f v e))]
        [(pNot e)        (dnot (fed f v e))]
        [(DP c (pVar s)) (fd c f v (dlkup v s))]
        [(δP c (pVar s)) (fδ f (dlkup v s))]
        [(DP c e)        (fd c f v e)]
        [(δP c e)        (fδ f e)]
        [e  e]
     )
  )


(define (fiterate-expand [f : FTable] [v : (ListEnv DPE)] [e : DPE] ) : DPE
        (cond
            [(dpe-pending? e) (fiterate-expand f v (fed f v e))]
            [else e])
)

(define (fstep-derivate  [fuel : Natural] [f : FTable] [v : (ListEnv DPE) ] [e : DPE] ) : (Listof DPE)
    (cond
      [(or (<= fuel 0) (not (dpe-pending? e))) (list e)]
      [else (cons e (fstep-derivate (- fuel 1) f v (fed f v e)) )]
  )
)

(define (fstep-expand  [fuel : Natural] [f : FTable] [v : (ListEnv DPE) ] [e : DPE] )
   (for ([x  (fstep-derivate fuel f v e) ])
        (pprint-dpe x)
        (displayln " ")
   )
)

(define (fstep-der  [fuel : Natural] [f : FTable] [c : Char] [e : DPEG ] )
   (for ([x  (fstep-derivate fuel f (DPEG-dv e) (fd c f (DPEG-dv e) (DPEG-ds e))) ])
        (pprint-dpe x)
        (displayln " ")
   )
)

; Derivate the PEG in relation to character c. 
;
;
(define (fderivate [c : Char] [g : DPEG] ) : DPE
   (let ([f : FTable (dpeg-first g)]) 
        (fiterate-expand f (DPEG-dv g) (fd c f (DPEG-dv g) (DPEG-ds g)))
   )
  )

(define (fderivate-dpe [c : Char] [f : FTable] [v : (ListEnv DPE)] [e : DPE] ) : DPE
   (fiterate-expand f v (fd c f v e))
  )

(define (fderivate-grammar [c : Char] [f : FTable] [g : DPEG] ) : DPEG
  (DPEG (DPEG-dv g)
         (fiterate-expand f (DPEG-dv g) (fd c f (DPEG-dv g) (DPEG-ds g)))
  )
)
(define (fderivateWith [c : Char] [f : FTable] [g : (ListEnv DPE)] [e : DPE] ) : DPE
   (fiterate-expand f g (fd c f g e))
  )



