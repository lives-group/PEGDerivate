#lang typed/racket
(require typed-racket-datatype)
(require "./opt.rkt")
(require "./env.rkt")
(require "./ftable.rkt")
(require "./boolsolver.rkt")


(provide DPE
         (struct-out pSym)
         (struct-out p∅)
         (struct-out pϵ)
         (struct-out p?)
         (struct-out pVar)
         (struct-out pCat)
         (struct-out pAlt)
         (struct-out pNot)
         (struct-out pKle)
         (struct-out δP)
         (struct-out DP)
         (struct-out DPEG)
         dlkup
         kle-remove
         dpe=?
         dpe-pending?
         dpe-null?
         ; Smart constructors
         dcat
         dalt
         dnot
         dkle
         
         alphabet
         alphabet-from-env
         alphabet-from-grammar

         ; Frist table
         quick-dpe-null?
         dpeg-first
         first

         ; Pretty print 
         dpe->string
         dpeg->string
         pprint-dpe
         pprint-dpeg)



; Parsing Expressions Derivates Definitions
; A mirror defininitio of PE, except for the adition for
; tracking pending derivate and delta computations.
(define-datatype DPE (pϵ)
                     (p∅)
                     (p?)
                     (pSym [c : Char])
                     (pVar [s : String])
                     (pCat [l : DPE] [r : DPE])
                     (pAlt [l : DPE] [r : DPE])
                     (pNot [p : DPE])
                     (pKle [p : DPE])      
                     (δP [b : Char]   [dp : DPE]) ; A pending Delta combinator operation
                     (DP [b : Char]   [dp : DPE]) ; A pending derivate operation
  )

(define-datatype DPEG (DPEG [dv : (ListEnv DPE)] [ds : DPE] ))




(define (dpe=? [e : DPE] [d : DPE] ) : Boolean
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

(define (dpe-pending? [e : DPE] ) : Boolean
     (match e
         [(p∅)           #f]
         [(p?)            #f]
         [(pϵ)            #f]
         [(pSym c)        #f]
         [(pVar s)        #f]
         [(pCat p1 p2)    (or (dpe-pending? p1) (dpe-pending? p2))]
         [(pAlt p1 p2)    (or (dpe-pending? p1) (dpe-pending? p2))]
         [(pKle p)        (dpe-pending? p)]
         [(pNot p)        (dpe-pending? p)]
         [(DP c p)        #t]
         [(δP c p)        #t]
     )
)


(define (dpe-null?  [v : (ListEnv DPE) ] [e : DPE]) : Boolean
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


(define (alphabet [e : DPE] ) :  (Listof Char)
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

(define (alphabet-from-env [d : (ListEnv DPE) ] ) :  (Listof Char)
    (union-list (map (lambda ([x :(Pair String DPE) ]) (alphabet (cdr x)) )  d))
)

(define (alphabet-from-grammar [d : DPEG ] ) :  (Listof Char)
            (set-union (alphabet (DPEG-ds d)) (alphabet-from-env (DPEG-dv d)))
)

(define (dlkup [v : (ListEnv DPE)] [s : String]) : DPE
      (match (lkp v s)
        [(None) (error (string-append "Undefined non-terminal " s ))]
        [(Some r) r]))


; Smart Constructos
  
(define (dcat [l : DPE] [r : DPE])
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

(define (dalt [l : DPE] [r : DPE]  ) : DPE
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

(define (dnot [l : DPE]) : DPE
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


(define (dkle [l : DPE]) : DPE
     (match l
        [(p∅)           (p∅) ]
        [(pϵ)            (pϵ)]
        [(pKle (pKle e)) (pKle e)]
        [e (pKle e)]
       )
  )


;; Klenne operation removal


(define (kle-rem [n : Natural ] [prefix : String] [v : (ListEnv DPE)]  [p : DPE] ) : (Values Natural DPE (ListEnv DPE))
  (match p
     [(p∅) (values n (p∅) v)]
     [(p?) (values n (p?) v)]
     [(pϵ) (values n (pϵ) v)]
     [(pSym c) (values n (pSym c) v)]
     [(pVar s) (values n (pVar s) v)]
     [(pCat p1 p2) (begin
                    (define-values (nat1 erm1 rs1) (kle-rem n prefix v p1))
                    (define-values (nat2 erm2 rs2) (kle-rem nat1 prefix rs1 p2))
                    (values nat2 (pCat erm1 erm2) rs2)
                   )]
     [(pAlt p1 p2) (begin
                    (define-values (nat1 erm1 rs1) (kle-rem n prefix v p1))
                    (define-values (nat2 erm2 rs2) (kle-rem nat1 prefix v p2))
                    (values nat2 (pAlt erm1 erm2) rs2)
                   )]
     [(pKle p) (begin
                    (define-values (nat1 erm1 rs1) (kle-rem n prefix v p))
                    (let* ([rname : String (string-append prefix (number->string nat1) )]
                           [prm : DPE (pAlt (pCat erm1 (pVar rname)) (pϵ)) ])
                           (values (+ nat1 1) (pVar rname) (append rs1 (list (cons rname prm))))
                     )
              )]
     [(pNot p) (begin
                 (define-values (nat erm rs) (kle-rem n prefix v p) )
                 (values nat (pNot erm) rs)
               )]
  )
)

(define (kle-rem-hlp  [prefix : String]
                      [x : (Pair String DPE)]
                      [setr : (Pair (ListEnv DPE) Natural)] ) : (Pair (ListEnv DPE) Natural)
    (define-values (nat2 p2 rs2) (kle-rem (cdr setr) prefix (car setr) (cdr x)))
    (cons (cons (cons (car x) p2) rs2) nat2 )
  )

(define (kle-rem-rules [n : Natural] [prefix : String] [v : (ListEnv DPE)]) : (ListEnv DPE)
   (car (foldr (lambda ([rule : (Pairof String DPE)] [nset : (Pairof (ListEnv DPE) Natural )])
                  (kle-rem-hlp prefix rule nset)
               )
               (cons null n)
              v
        ))
)

(define (kle-remove [g : DPEG] ) : DPEG
        (define-values (nat p rs) (kle-rem 0 "k_" null (DPEG-ds g)))
        (DPEG (kle-rem-rules nat "k_" (append (DPEG-dv g) rs)) p )
  )



(define (first  [Σ : (Listof Char)] [ l : FTable] [p : DPE]  ) :  (Listof Char)
     (match p
        [(p∅)           null]
        [(p?)            Σ]
        [(pϵ)            null]
        [(pSym c)        (list c)]
        [(pVar s)        (ft-get l s)]
        [(pCat p1 p2)    (cond [(quick-dpe-null? l p1) (set-union (first Σ l p1) (first Σ l p2))]
                               [else             (first Σ l p1) ]) ]
        [(pAlt p1 p2)    (set-union (first Σ l p1) (first Σ l p2))]
        [(pKle p)        (first Σ l p)]
        [(pNot p)        (first Σ l p)] 
        [_               null]
     )
  )


(define (iterate-first-table  [Σ : (Listof Char)] [ l : FTable]  [v : (ListEnv DPE) ] ) :  FTable
  (let ([tab : FTable (foldr (lambda ([x : (Pair String DPE)] [t : FTable])  (ft-ins-all t (car x) (first Σ t (cdr x)) ) ) l v) ])
       (cond [(ft-changed? tab) (iterate-first-table Σ (ft-rst tab) v)]
             [else tab])
  )
)




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


(define (ft-from-grm [v : (ListEnv DPE)]) : FTable
    (let* ([names : (Listof String) (map (lambda ([k : (Pair String DPE)]) (car k)) v)]
           [tb : FTable (mk-empty-ftable names)]
           [nll : (Listof (Pair String Boolean))
                 (dpe-env-null v)])
           (foldr (lambda ([e : (Pair String Boolean)] [f : FTable]) (ft-set-nullable f (car e) (cdr e))) tb nll)
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



;Primary     5
;Kle         4
;Not         3
;Sequence    2 Left
;alternative 1 Left
(define (dpe-prec->string [n : Natural ] [e : DPE]) : String
    (match e
        [(p∅)           "∅"]
        [(p?)            "."]
        [(pϵ)            "ϵ"]
        [(pSym c)        (string c)]
        [(pVar s)        s]
        [(pCat p1 p2)    (parens (> n 2) (string-append (dpe-prec->string 2 p1) (dpe-prec->string 2 p2)))]
        [(pAlt p1 p2)    (parens (> n 1) (string-append (dpe-prec->string 1 p1) "/" (dpe-prec->string 1 p2)))]
        [(pKle p)        (parens (> n 4) (string-append (dpe-prec->string 4 p) "*"))  ]
        [(pNot p)        (parens (> n 3) (string-append "!" (dpe-prec->string 3 p) )) ]
        [(DP c p)        (string-append "d(" (string c) ", " (dpe-prec->string 0 p) ")")]
        [(δP c p)        (string-append "δ(" (string c) ", " (dpe-prec->string 0 p) ")")]
     )
  )

(define (parens [b : Boolean] [s : String]) : String
     (match b
           [#f   s]
           [else  (string-append "(" s ")")]
  )
)

(define (dpe->string [e : DPE]) : String
    (dpe-prec->string 0 e))

(define (dpeg->string [e : DPEG]) : (Listof String)
    (append (env-trasform (lambda ([s : String] [exp : DPE]) (string-append s "<-" (dpe-prec->string 0 exp))) (DPEG-dv e))
            (list (dpe-prec->string 0 (DPEG-ds e)))
    )
)

(define (pprint-dpe [e : DPE])
   (display (dpe-prec->string 0 e))
 )

(define (pprint-dpeg [e : DPEG])
    (for ([s  (dpeg->string e)])
         (displayln s)
    )
)
