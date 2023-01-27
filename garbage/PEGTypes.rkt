#lang typed/racket
(require typed-racket-datatype)
(require rackcheck)
(provide (all-defined-out))

; Start the pseud random generator
(current-pseudo-random-generator  (make-pseudo-random-generator)) 

; PEG AST definition. 
; Parsing Expressions Definitions
(define-datatype PE (Eps)
                    (∅)
                    (Any)
                    (Sym [c : Char])
                    (Var [v : String])
                    (Cat [l : PE] [r : PE])
                    (Alt [l : PE] [r : PE])
                    (Kle [e : PE])
                    (Not [e : PE])
  )

; A PEG grammar is comprised of a List of associations of names (String)
; to 
(define-datatype PEG (PEG [v : (Listof (Pair String PE))]
                          [s : PE] ) )
                    
(define (lkup [v : (Listof (Pair String PE))] [s : String]) : PE
      (match v
        ['() (error (string-append "Undefined non-terminal " s))]
        [(cons (cons n p) xs) (cond
                                [(eq? n s) p]
                                [else (lkup xs s)])]))


(define (δ [a : Char] [v : (Listof (Pair String PE))] [p : PE] ) : PE
     (match p
        [(∅)         (∅)]
        [(Any)        (Any)]
        [(Eps)        (Eps)]
        [(Sym _)      (∅)]
        [(Var s)      (δ a v (lkup v s))]
        [(Cat p1 p2)  (cat  (δ a v p1) (δ a v p2))]
        [(Alt p1 p2)  (alt  (δ a v p1) (cat (δ a v (notPE p1)) (δ a v p2)))]
        [(Kle p)      (alt (cat (δ a v p) (Kle p)) (cat (δ a v (notPE p)) (Kle p) ))] ;; Diverde do artigo
        [(Not p)      (notPE (D a v (cat p (Kle (Any)) )))]
       )
  )


(define (D [a : Char] [v : (Listof (Pair String PE))] [p : PE] ) : PE
     (match p
        [(∅)         (∅)]
        [(Any)        (Eps)]
        [(Eps)        (∅)]
        [(Sym c)      (cond [(eq? c a) (Eps)] [else (∅) ])]
        [(Var s)      (D a v (lkup v s))]
        [(Cat p1 p2)  (alt (cat  (D a v p1)  p2) (cat  (δ a v p1)  (D a v p2)))]
        [(Alt p1 p2)  (alt  (D a v p1) (D a v p2))]
        [(Kle p)      (alt (cat (D a v p) (Kle p)) (cat (δ a v p) (D a v (Kle p)) ))] ;; Entrar em loop
        [(Not p)      (∅)]
       )
  )

(define (lam [v : (Listof (Pair String PE))] [p : PE]) : Boolean
   (match p
     [(∅) #f]
     [(Eps) #t]
     [(Sym _) #f]
     [(Var s) (lam v (lkup v s))]
     [(Cat p1 p2) (and (lam v p1) (lam v p2))]
     [(Alt p1 p2) (or (lam v p1) (lam v p2))]
     [(Kle p) #t]
     [(Not p) #t]
  )
)

(define (nullable [v : (Listof (Pair String PE))] [p : PE]) : Boolean
   (match p
     [(∅) #f]
     [(Eps) #t]
     [(Sym _) #f]
     [(Var s) (lam v (lkup v s))]
     [(Cat p1 p2) (and (lam v p1) (lam v p2))]
     [(Alt p1 p2) (or (lam v p1) (lam v p2))]
     [(Kle p) #t]
     [(Not p) #f]
  )
)

; Smart Constructos

(define (cat [l : PE] [r : PE])
     (match (cons l r)
        [(cons (∅) _ )  (∅) ]
        [(cons _  (∅)) (∅) ]
        [(cons (Eps) d) d ]
        [(cons e (Eps)) e ]
        [(cons e d) (Cat e d )]
       )
  )

(define (alt [l : PE] [r : PE] ) : PE
     (match (cons l r)
        [(cons (∅) d )    d ]
        [(cons e (∅))     e ]
        [(cons e d) (Alt e d)]
       )
  )

(define (notPE [l : PE] ) : PE
     (match l
        [(∅)    (Not (∅))]
        [e (Not e)]
       )
  )


;---------------------------------------------------------------
;      SAMPLE PEG EXAMPLES
;---------------------------------------------------------------

;pex0 S <- (ab)
(define pex0 : PEG
    (PEG (list (cons "S"  (Cat (Sym #\a) (Sym #\b) )  ))
         (Var "S")) 
  )

(define pex1 : PEG
    (PEG (list (cons "S" (Alt (Cat (Sym #\a) (Var "S") ) (Eps) )))
         (Var "S")) 
  )

(define pex2 : PEG
    (PEG (list (cons "S" (Alt (Cat (Sym #\a) (Cat (Var "S") (Sym #\b) ) ) (Eps))))
         (Var "S")) 
  )
