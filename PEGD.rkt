#lang typed/racket
(require typed-racket-datatype)
(require rackcheck)
(provide (all-defined-out))

; Start the pseud random generator
(current-pseudo-random-generator  (make-pseudo-random-generator)) 

(define (compose-gen [ xs : (Listof Natural)] [ ys : (Listof Natural)] ) : (Listof Natural)
     (match ys
        ['() '()]
        [(cons i ws) (cond
                        [(< i (length xs)) (cons (list-ref xs i) (compose-gen xs ws)) ]
                        [else '()] ) ]
       )   
  )

(define (ins [x : Natural ] [ ys : (Listof Natural)] ) : (Listof Natural)
     (match ys
        ['() (list x)]
        [(cons i ws) (cond
                        [(< x i) (cons x (cons i ws)) ]
                        [else (cons i (ins x ws)) ] ) ]
       )   
  )

(define (insPE [x : Natural ] [p : PE] [ ys : (Listof (Pair Natural PE))] ) :  (Listof (Pair Natural PE))
     (match ys
        ['() (list (cons x p))]
        [(cons (cons i q) ws) (cond
                        [(< x i) (cons (cons x p) (cons (cons i q) ws)) ]
                        [else (cons (cons i q) (insPE x p ws)) ] ) ]
       )   
  )

(define (updatePE [x : Natural ] [p : PE] [ ys : (Listof (Pair Natural PE))] ) :  (Listof (Pair Natural PE))
     (match ys
        ['() (list (cons x p))]
        [(cons (cons i q) ws) (cond
                        [(eq? x i) (cons (cons x p) (cons (cons i q) ws)) ]
                        [else (cons (cons i q) (updatePE x p ws)) ] ) ]
       )   
  )


(define (lkupPE [n : Natural] [ ys : (Listof (Pair Natural PE))]) :  PE
    (match ys
      ['() (error (string-append "undefined index: " (number->string n)))]
      [(cons (cons k p) zs) (cond
                              [(eq? k n) p]
                              [else (lkupPE n zs)]) ])
)

(define (proj [n : (Listof Natural)] [ ys : (Listof (Pair Natural PE))]) : (Listof  PE)
    (map (lambda ([x : (Pair Natural PE)]) (cdr x))
     (filter (lambda ([x : (Pair Natural PE)]) (member (car x) n ) ) ys))
)

(define (uni-all [ys : (Listof (Listof Natural))] ) : (Listof  Natural)
    (foldr (lambda ([x : (Listof Natural)] [ws : (Listof Natural)]) (set-union x ws))
     '() ys)
)

(define-datatype PE (Eps [ann :  Natural] )
                    (∅)
                    (∞)
                    (Sym [c : Char])
                    (Var [v : String])
                    (Cat [l : PE] [r : PE] [ann : (Listof (Pair Natural PE))])
                    (Alt [l : PE] [r : PE])
                    (Kle [e : PE])
                    (Not [ann : Natural] [e : PE])
  )

(define-datatype PEG (PEG [v : (Listof (Pair String PE))]
                          [s : PE] ) )
                    
(define (lkup [v : (Listof (Pair String PE))] [s : String]) : PE
      (match v
        ['() (error (string-append "Undefined non-terminal " s))]
        [(cons (cons n p) xs) (cond
                                [(eq? n s) p]
                                [else (lkup xs s)])]))


(define (lam [v : (Listof (Pair String PE))] [p : PE]) : Boolean
   (match p
     [(∅) #f]
     [(Eps _) #t]
     [(Sym _) #f]
     [(Var s) (lam v (lkup v s))]
     [(Cat p1 p2 ann) (and (lam v p1) (lam v p1))]
     [(Alt p1 p2) (or (lam v p1) (lam v p1))]
     [(Kle p) #t]
     [(Not an p) #t]
  )
)

(define (nullable [v : (Listof (Pair String PE))] [p : PE]) : Boolean
   (match p
     [(∅) #f]
     [(Eps _) #t]
     [(Sym _) #f]
     [(Var s) (lam v (lkup v s))]
     [(Cat p1 p2 ann) (and (lam v p1) (lam v p1))]
     [(Alt p1 p2) (or (lam v p1) (lam v p1))]
     [(Kle p) #t]
     [(Not an p) #f]
  )
)

(define (ann [ n : Natural] [ p : PEG ]) : PEG
  (match p
    [(PEG v peg) (PEG v (annPE n v peg)) ]
  ))

(define (matchPE [ p : PE]) : (Listof Natural)
  (match p
      [(Eps a) (list a) ]
      [(∅)    '() ]  
      [(Sym _) '() ]
      [(Not a _) '()]
      [(Cat e d xs) (uni-all (map matchPE (proj (matchPE e) xs)))]
      [(Alt e d) (matchPE d)]
      )
  )

(define (backPE [ p : PE]) : (Listof Natural)
  (match p
      [(Eps a) (list a) ]
      [(∅)    '() ]  
      [(Sym _) '() ]
      [(Not a _) (list a)]
      [(Cat e d xs) (uni-all (map (lambda ([x : (Pair Natural PE)]) (backPE (cdr x))) xs))]
      [(Alt e d) (set-union (backPE e) (backPE d))]
      )
  )

(define (annPE [n : Natural ] [v : (Listof (Pair String PE))] [ p : PE ]) : PE
  (match p
    [(∅) (∅)]
    [(Eps x) (Eps n)]
    [(Sym c) (Sym  c)]
    [(Var s) (annPE n v (lkup v s))]
    [(Not x e) (Not n (annPE n v e))]
    ;[(Kle p) (??)] Não é suposto aparecer !
    [(Alt e d) (Alt (annPE n v e) (annPE n v d))]
    [(Cat e d xs) (Cat (annPE n v e) d (cond [(lam v d) (insPE n d xs)] [else xs]))]
  )
 )

(define (der [c : Char] [i : Natural] [ v : (Listof (Pair String PE)) ] [ p : PE ]) : PE
  (match p
    [(∅) (∅)]
    [(Eps x) (Eps x)]
    [(Sym ch) (cond [(eq? c ch) (Eps i)]
                    [else (∅)])]
    [(Var s) (annPE i v (lkup v s))]
    ;[(Not x e) (Not n (annPE n v e))]
    ;[(Kle p) (??)] Não é suposto aparecer !
    [(Alt e d) (let ([de (der c i v e)]
                     [dd (der c i v d)])
                    (cond
                      [(not (null? (matchPE de))) de ]
                      [else (alt de dd v)]) )]
    [(Cat e d xs) (let* ([de (der c i v e) ]
                         [bk (backPE de)])
                        (match de
                           [(∅) (∅) ]
                           [(Eps k) (cond
                                      [(and (eq? k i) (not (eq? c #\#))) (annPE i v d) ]
                                      [(and (eq? k i) (eq? c #\#)) (der c i v (annPE i v d)) ]
                                      [(< k i) (der c i v (lkupPE k xs)) ]
                                      [else (error "undefined sequence condition")] )]
                           [ _  (cat de d (map (lambda ([jp : (Pair Natural PE)]) (cons (car jp) (der c i v (cdr jp))))  xs))]))
    ]
  )
 )

; Smart Constructos

(define (cat [l : PE] [r : PE] [zs : (Listof (Pair Natural PE))])
     (match (cons l r)
        [(cons (∅) _ )  (∅) ]
        [(cons _  (∅)) (∅) ]
        [(cons (Eps _) d) d ]
        [(cons e (Eps _)) e ]
        [(cons e d) (Cat e d zs)]
       )
  )

(define (alt [l : PE] [r : PE]  [zs : (Listof (Pair String PE))] ) : PE
     (match (cons l r)
        [(cons (∅) d )    d ]
        [(cons e (∅))     e ]
        [(cons e d)  (cond
                       [(nullable zs e) d]
                       [else (Alt e d)]) ]
       )
  )

(define pex0 : PEG
    (PEG (list (cons "S"  (Cat (Sym #\a) (Sym #\b) '())  ))
         (Var "S")) 
  )

(define pex1 : PEG
    (PEG (list (cons "S" (Alt (Cat (Sym #\a) (Var "S") '()) (Eps 0 ) )))
         (Var "S")) 
  )

(define pex2 : PEG
    (PEG (list (cons "S" (Alt (Cat (Sym #\a) (Cat (Var "S") (Sym #\b) '()) '()) (Eps 0 ))))
         (Var "S")) 
  )


(define (stepDer [g : PEG] [s : String ]) : (Listof (Pair Char PE))
    (stepDerAux 0 (PEG-v g) (annPE 0 (PEG-v g) (PEG-s g)) s)
  )

(define (stepDerAux [i : Natural] [v : (Listof (Pair String PE))]  [p : PE] [s : String]) : (Listof (Pair Char PE))
     (cond
       [(>= i (string-length s)) null ] 
       [else (let* ([c (string-ref s i)]
                    [pd (der c i v p)])
                   (cons (cons c pd) (stepDerAux (+ i 1) v pd s))) ])

  )