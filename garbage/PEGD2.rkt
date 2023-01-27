#lang typed/racket
(require typed-racket-datatype)
; (require)
(provide (all-defined-out))

; Start the pseud random generator
(current-pseudo-random-generator  (make-pseudo-random-generator)) 

; PEG AST definition. 
; Parsing Expressions Definitions
(define-datatype PE (∅)
                    (Eps)
                    (Any)
                    (Sym [c : Char])
                    (Var [v : String])
                    (Cat [l : PE] [r : PE])
                    (Alt [l : PE] [r : PE])
                    (Kle [e : PE])
                    (Not [e : PE])
  )

(define-type RuleSet (Listof (Pair String PE) ))
(define-datatype PEG (PEG [v : RuleSet]
                          [s : PE] ) )
                    
(define (lkup [v : RuleSet] [s : String]) : PE
      (match v
        ['() (error (string-append "Undefined non-terminal " s))]
        [(cons (cons n p) xs) (cond
                                [(string=? n s) p]
                                [else (lkup xs s)])]))


(define (lam [v : RuleSet] [p : PE]) : Boolean
   (match p
     [(∅) #f]
     [(Any) #f]
     [(Eps) #t]
     [(Sym _) #f]
     [(Var s) (lam v (lkup v s))]
     [(Cat p1 p2) (and (lam v p1) (lam v p2))]
     [(Alt p1 p2) (or (lam v p1) (lam v p2))]
     [(Kle p) #t]
     [(Not p) #t]
  )
)


(define (pe=? [e : PE] [d : PE] ) : Boolean
   (match (cons e d)
     [(cons (∅)                   (∅)) #t]
     [(cons (Eps)                  (Eps)) #t]
     [(cons (Any)                  (Any)) #t]
     [(cons (Sym c)   (Sym c1))    (char=? c c1)]
     [(cons (Var s1)  (Var s2))    (string=? s1 s2)]
     [(cons (Cat l r) (Cat l2 r2)) (and (pe=? l l2) (pe=? r r2))]
     [(cons (Alt l r) (Alt l2 r2)) (and (pe=? l l2) (pe=? r r2))]
     [(cons (Not l)   (Not l2))    (pe=? l l2)]
     [(cons (Kle l)   (Kle l2))    (pe=? l l2)]
     [(cons _ _ )                    #f]
     )
  )

(define (nullable [v : RuleSet] [p : PE]) : Boolean
   (match p
     [(∅) #f]
     [(Any) #f]
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
        [(cons e d) (Cat e d)]
       )
  )

(define (alt [l : PE] [r : PE]  ) : PE
     (match (cons l r)
        [(cons (∅) d ) d ]
        [(cons e (∅))  e ]
        [(cons e d)     (Alt e d) ]
       )
  )



(define (kle-rem [n : Natural ] [prefix : String] [v : RuleSet] [p : PE] ) : (Values Natural PE RuleSet)
  (match p
     [(∅) (values n (∅) v)]
     [(Any) (values n (Any) v)]
     [(Eps) (values n (Eps) v)]
     [(Sym c) (values n (Sym c) v)]
     [(Var s) (values n (Var s) v)]
     [(Cat p1 p2) (begin
                    (define-values (nat1 erm1 rs1) (kle-rem n prefix v p1))
                    (define-values (nat2 erm2 rs2) (kle-rem nat1 prefix rs1 p2))
                    (values nat2 (Cat erm1 erm2) rs2)
                   )]
     [(Alt p1 p2) (begin
                    (define-values (nat1 erm1 rs1) (kle-rem n prefix v p1))
                    (define-values (nat2 erm2 rs2) (kle-rem nat1 prefix v p2))
                    (values nat2 (Alt erm1 erm2) rs2)
                   )]
     [(Kle p) (begin
                    (define-values (nat1 erm1 rs1) (kle-rem n prefix v p))
                    (let* ([rname : String (string-append prefix (number->string nat1) )]
                           [prm : PE (Alt (Cat erm1 (Var rname)) (Eps)) ])
                           (values (+ nat1 1) (Var rname) (append rs1 (list (cons rname prm))))
                     )
              )]
     [(Not p) (begin
                 (define-values (nat erm rs) (kle-rem n prefix v p) )
                 (values nat (Not erm) rs)
               )]
  )
)

(define (kle-rem-hlp  [prefix : String]
                      [x : (Pair String PE)]
                      [setr : (Pair RuleSet Natural)] ) : (Pair RuleSet Natural)
    (define-values (nat2 p2 rs2) (kle-rem (cdr setr) prefix (car setr) (cdr x)))
    (cons (cons (cons (car x) p2) rs2) nat2 )
  )

(define (kle-rem-rules [n : Natural] [prefix : String] [v : RuleSet]) : RuleSet
   (car (foldr (lambda ([rule : (Pairof String PE)] [nset : (Pairof RuleSet Natural )])
                  (kle-rem-hlp prefix rule nset)
               )
               (cons null n)
              v
        ))
)

(define (kle-remove [g : PEG] ) : PEG
        (define-values (nat p rs) (kle-rem 0 "k_" null (PEG-s g)))
        (PEG (kle-rem-rules nat "k_" (append (PEG-v g) rs)) p )
  )


; S <- ab 
(define pex0 : PEG
    (PEG (list (cons "S"  (Cat (Sym #\a) (Sym #\b) )  ))
         (Var "S")) 
  )

; S <- aS / Eps
(define pex1 : PEG
    (PEG (list (cons "S" (Alt (Cat (Sym #\a) (Var "S") ) (Eps) )))
         (Var "S")) 
  )
; S <- aSb / ϵ
(define pex2 : PEG
    (PEG (list (cons "S" (Alt (Cat (Sym #\a) (Cat (Var "S") (Sym #\b))) (Eps))))
         (Var "S")) 
  )

; S <- a*
(define pex3 : PEG
    (PEG (list (cons "S" (Kle (Sym #\a))) )
         (Var "S")) 
  )

; S <- aSb / ϵ
(define pex4 : PEG
    (PEG (list (cons "A" (Alt (Cat (Sym #\a) (Var "A")) (Eps) ))
               (cons "B" (Alt (Cat (Sym #\b) (Var "B")) (Sym #\c) ))
               )
         (Cat (Var "A") (Var "B" ))) 
  )

; S <- aSb / ϵ
(define pex5 : PEG
    (PEG (list (cons "A" (Cat (Sym #\b) ( Kle (Sym #\b)) )))
         (Cat (Kle (Sym #\a)) (Kle (Sym #\b)) )) 
  )


; A <- aA / ϵ
; B <- AB / BA
; AB
(define pex6 : PEG
    (PEG (list (cons "A" (Alt (Cat (Sym #\a) ( Var "A") )  (Eps) ))
               (cons "B" (Alt (Cat (Var "A") (Var "B")) (Cat (Var "B") (Var "A")))) )
         (Cat (Var "A") (Var "B") )) 
  )