#lang typed/racket
(require typed-racket-datatype)
(require "./PEGD2.rkt")
(require "./opt.rkt")
(require "./env.rkt")
(require "./boolsover.rkt")
(require "./ftable.rkt")

(provide (all-defined-out))



; Parsing Expressions Derivates Definitions
; A mirror defininitio of PE, except for the adition for
; tracking pending derivate and delta computations.
(define-datatype dPE (pϵ)
                     (p∅)
                     (p?)
                     (pSym [c : Char])
                     (pVar [s : String])
                     (pCat [l : dPE] [r : dPE])
                     (pAlt [l : dPE] [r : dPE])
                     (pNot [p : dPE])
                     (pKle [p : dPE])      
                     (δP [b : Char]   [dp : dPE]) ; A pending Delta operation
                     (DP [b : Char]   [dp : dPE]) ; A pending derivate operation
  )


(define-datatype DPEG (DPEG [dv : (ListEnv dPE)] [ds : dPE] ))




(define (dpe=? [e : dPE] [d : dPE] ) : Boolean
   (match (cons e d)
     [(cons (p∅) (p∅)) #t]
     [(cons (pϵ)  (pϵ))  #t]
     [(cons (p?)  (p?))  #t]
     [(cons (pSym c)  (pSym c1))     (char=? c c1)]
     [(cons (pVar s1)  (pVar s2))    (string=? s1 s2)]
     [(cons (pCat l r) (pCat l2 r2)) (and (dpe=? l l2) (dpe=? r r2))]
     [(cons (pAlt l r) (pAlt l2 r2)) (and (dpe=? l l2) (dpe=? r r2))]
     [(cons (pNot l)   (pNot l2))    (dpe=? l l2)]
     [(cons (pKle l)   (pKle l2))    (dpe=? l l2)]
     [(cons (δP c l) (δP c1 l1))     (and (char=? c c1) (dpe=? l l1))]
     [(cons (DP c l) (DP c1 l1))     (and (char=? c c1) (dpe=? l l1))]
     [(cons _ _ )                    #f]
     )
  )

(define (dpe-pending? [e : dPE] ) : Boolean
     (match e
         [(p∅)           #f]
         [(p?)            #f]
         [(pϵ)            #f]
         [(pSym c)        #f]
         [(pVar s)        #f]
         [(pCat p1 p2)    (or (dpe-pending? p1) (dpe-pending? p2))]
         [(pAlt p1 p2)    (or (dpe-pending? p2) (dpe-pending? p2))]
         [(pKle p)        (dpe-pending? p)]
         [(pNot p)        (dpe-pending? p)]
         [(DP c p)        #t]
         [(δP c p)        #t]
     )
)


(define (dpe-null?  [v : (ListEnv dPE) ] [e : dPE]) : Boolean
       (match e
        [(p∅)           #f]
        [(p?)            #f]
        [(pϵ)            #t]
        [(pSym c)        #f]
        [(pVar s)        (dpe-null? v (dlkup v s))]
        [(pCat p1 p2)    (let ([r : Boolean (dpe-null? v p1)] )
                              (cond [r (dpe-null? v p2)] [else r]))]
        [(pAlt p1 p2)     (let ([r : Boolean (dpe-null? v p1)] )
                               (cond [(not r) (dpe-null? v p2)] [else r]))] ;(tor (dpe-null? v p1) (dpe-null? v p2))]
        [(pKle p)        #t]
        [(pNot p)        (not (dpe-null? v p))] ; This needs not to be lazy here !
        [_        (error "tried to determine if a peding operation is null !")]
     )
  )

(define (dpe-exp-null  [e : dPE ] ) : CExp
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

(define (dpe-env-null  [v : (ListEnv dPE) ] ) : (ListEnv Boolean)
      (env-map isTrue? (solve-env (env-map (lambda ([x : dPE])  (dpe-exp-null x)) v)))
  )

(define (alphabet [e : dPE] ) :  (Listof Char)
     (match e
         [(p∅)           null]
         [(p?)            null]
         [(pϵ)            null]
         [(pSym c)        (list c)]
         [(pVar s)        null]
         [(pCat p1 p2)    (set-union (alphabet p1) (alphabet p2))]
         [(pAlt p1 p2)    (set-union (alphabet p1) (alphabet p2))]
         [(pKle p)        (alphabet p)]
         [(pNot p)        (alphabet p)]
         [(DP c p)        (alphabet p)]
         [(δP c p)        (alphabet p)]
     )
)

(define (union-list [l : (Listof (Listof Char))] ) : (Listof Char)
     (foldr (lambda ([a :(Listof Char)] [b :(Listof Char)]) (set-union a b)) null l)
  )

(define (alphabetRuleSet [d : (ListEnv dPE) ] ) :  (Listof Char)
    (union-list (map (lambda ([x :(Pair String dPE) ]) (alphabet (cdr x)) )  d))
)

(define (alphabetG [d : DPEG ] ) :  (Listof Char)
            (set-union (alphabet (DPEG-ds d)) (alphabetRuleSet (DPEG-dv d)))
)

(define (ft-from-grm [v : (ListEnv dPE)]) : FTable
    (let* ([names : (Listof String) (map (lambda ([k : (Pair String dPE)]) (car k)) v)]
           [tb : FTable (mk-empty-ftable names)]
           [nll : (Listof (Pair String Boolean))
                 (dpe-env-null v)])
           (foldr (lambda ([e : (Pair String Boolean)] [f : FTable]) (ft-set-nullable f (car e) (cdr e))) tb nll)
     )
  )


(define (first1  [Σ : (Listof Char)] [ l : FTable] [p : dPE]  ) :  (Listof Char)
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


(define (iterate-first-table  [Σ : (Listof Char)] [ l : FTable]  [v : (ListEnv dPE) ] ) :  FTable
  (let ([tab : FTable (foldr (lambda ([x : (Pair String dPE)] [t : FTable])  (ft-ins-all t (car x) (first1 Σ t (cdr x)) ) ) l v) ])
       (cond [(ft-changed? tab) (iterate-first-table Σ (ft-rst tab) v)]
             [else tab])
  )
)


(define (first-dpeg  [ d : DPEG] ) :  FTable
     (iterate-first-table (alphabetG d) (ft-from-grm (DPEG-dv d) ) (DPEG-dv d))
)

(define (quick-dpe-null?  [ft : FTable ] [e : dPE]) : Boolean
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


(define (imprt  [p : PE]) : dPE
   (match p
     [(∅)    (p∅)]
     [(Any)   (p?)]
     [(Eps)   (pϵ)]
     [(Sym c) (pSym c)]
     [(Var s) (pVar s)]
     [(Cat p1 p2) (pCat (imprt p1) (imprt p2))]
     [(Alt p1 p2) (pAlt (imprt p1) (imprt p2))]
     [(Kle p) (pKle (imprt p))]
     [(Not p) (pNot (imprt p))]
  )
)

(define (imprtVars  [v : (ListEnv PE)]) :  (ListEnv dPE)
   (map (lambda ([p : (Pair String PE)] )  (cons (car p) (imprt (cdr p)) ) ) v)
)

(define (imprtPEG  [g : PEG]) :  DPEG
   (DPEG (imprtVars (PEG-v g))  (imprt (PEG-s g)))
)

;(: lkp (All (A) (-> String (Listof (Pair String A)) (Opt A)) ))
#;(define (lkp v s)
      (match v
        ['() (None)]
        [(cons (cons n p) xs) (cond
                                [(string=?  n s) (Some p)]
                                [else            (lkp xs s)])]))


(define (dlkup [v : (ListEnv dPE)] [s : String]) : dPE
      (match (lkp v s)
        [(None) (error (string-append "Undefined non-terminal " s ))]
        [(Some r) r]))


;
;
(define (δ [a : Char] [v : (ListEnv dPE)] [p : dPE]) :  dPE
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


(define (d [a : Char] [v : (ListEnv dPE)] [p : dPE]) :  dPE
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
(define (ed [v : (ListEnv dPE)] [p : dPE]) :  dPE
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
(define (ed1 [v : (ListEnv dPE)] [p : dPE]) :  dPE
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

; Smart Constructos
  
(define (dcat [l : dPE] [r : dPE])
     (match (cons l r)
        [(cons (p∅) d)  (p∅)]
        [(cons e (p∅))  (p∅)]
        [(cons (pϵ) d)   d]
        [(cons e (pϵ))   e]
        [(cons (pKle (p?)) (pKle (p?)))   (pKle (p?))]
        [(cons (pNot e) (pCat (pNot m) d)) (cond
                                             [(dpe=? e m) (pCat (pNot m) d)]
                                             [else (pCat (pNot e) (pCat (pNot m) d))])] 
        [(cons e d)        (pCat e d)]
       )
  )

(define (dalt [l : dPE] [r : dPE]  ) : dPE
     (match (cons l r)
        [(cons (p∅) d)                   d ]
        [(cons e     (p∅))               e ]
        [(cons e     (pCat (pNot e2) e3)) (cond
                                            [(dpe=? e e2) (pAlt e e3)]
                                            [else (pAlt e (pAlt (pNot e2) e3))]) ]
        [(cons e     (pNot e2))           (cond
                                            [(dpe=? e e2) (pAlt e (pϵ))]
                                            [else (pAlt e e2)]) ]
        [(cons (pCat e d) (pCat e1 d1))   (cond
                                            [(dpe=? e e1) (pCat e (pAlt d d1)) ]
                                            [else (pAlt (pCat e d) (pCat e1 d1)) ] ) ]
        [(cons (pCat e (pKle (p?))) (pCat e1 (pKle (p?))))  (pCat (pAlt e e1) (pKle (p?)))]
        [(cons e d)                       (cond
                                            [(dpe=? e d) e]
                                            [else (pAlt e d) ] ) ]
       )
  )

(define (dnot [l : dPE]) : dPE
     (match l
        [(p∅)        (pϵ) ]
        [(pϵ)         (p∅)]
        [(pKle (p?))  (p∅)]
        [(pKle (p?))  (p∅)]
        [(pCat e (pKle (p?)))   (pNot e)]
        [(pNot (pNot (pNot e))) (pNot e)]
        [e (pNot e)]
       )
  )



;Primary     5
;Kle         4
;Not         3
;Sequence    2 Left
;alternative 1 Left
(define (dpe-pprint [n : Natural ] [e : dPE]) : String
    (match e
        [(p∅)           "∅"]
        [(p?)            "."]
        [(pϵ)            "ϵ"]
        [(pSym c)        (string c)]
        [(pVar s)        s]
        [(pCat p1 p2)    (parens (> n 2) (string-append (dpe-pprint 2 p1) (dpe-pprint 2 p2)))]
        [(pAlt p1 p2)    (parens (> n 1) (string-append (dpe-pprint 1 p1) "/" (dpe-pprint 1 p2)))]
        [(pKle p)        (parens (> n 4) (string-append (dpe-pprint 4 p) "*"))  ]
        [(pNot p)        (parens (> n 3) (string-append "!" (dpe-pprint 3 p) )) ] ; This needs not to be lazy here !
        [(DP c p)        (string-append "d(" (string c) ", " (dpe-pprint 0 p) ")")]
        [(δP c p)        (string-append "δ(" (string c) ", " (dpe-pprint 0 p) ")")]
     )
  )

(define (parens [b : Boolean] [s : String]) : String
     (match b
           [#f   s]
           [else  (string-append "(" s ")")]
  )
)

(define (iterate-expand [v : (ListEnv dPE)] [e : dPE] ) : dPE
     (begin
        ;(println (dpe-pprint 0 e))
        (cond
            [(dpe-pending? e) (iterate-expand v (ed v e))]
            [else e]))
)

(define (step-derivate  [fuel : Natural]  [v : (ListEnv dPE) ] [e : dPE] ) : (Listof dPE)
    (cond
      [(or (<= fuel 0) (not (dpe-pending? e))) (list e)]
      [else (append (list e) (step-derivate (- fuel 1) v (ed v e)) )]
  )
)

(define (step-expand  [fuel : Natural] [v : (ListEnv dPE) ] [e : dPE] )
   (for ([x  (step-derivate fuel v e) ])
        (println (dpe-pprint 0 x))
   )
)

(define (step-der  [fuel : Natural] [c : Char] [e : DPEG ] )
   (for ([x  (step-derivate fuel (DPEG-dv e) (d c (DPEG-dv e) (DPEG-ds e))) ])
        (println (dpe-pprint 0 x))
   )
)

; Derivate the PEG in relation to character c. 
;
;
(define (derivate [c : Char] [g : DPEG] ) : dPE
   (iterate-expand (DPEG-dv g) (d c (DPEG-dv g) (DPEG-ds g)))
  )

(define (derivateWith [c : Char] [g : (ListEnv dPE)] [e : dPE] ) : dPE
   (iterate-expand g (d c g e))
  )




;(define t (pAlt (pCat (DP #\a (pSym #\a)) (pVar "A")) (pCat (δP #\a (pSym #\a)) (DP #\a (pVar "A"))) ))


; Step DERIVATE BUG !
; "d(a, AB)"
; "d(a, A)B                     / δ(a, A)d(a, B)"
; "d(a, aA/ϵ)B                  / δ(a, aA/ϵ)d(a, bB/c)"
; "(d(a, aA) / d(a, ϵ))B        / !d(a, aA.*)                    / d(a, bB)  / d(a, c)"
; "(d(a, a)A / δ(a, a)d(a, A))B / !(d(a, aA).*/δ(a, aA)d(a, .*)) / d(a, b)B / δ(a, b)d(a, B)"
; "AB/(d(a, a)A / δ(a, a)d(a, A)).*"
; "A(B/.*)"