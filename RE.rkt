#lang typed/racket
(require typed-racket-datatype)
(require rackcheck)
(provide (all-defined-out))

(current-pseudo-random-generator  (make-pseudo-random-generator)) 

(define-datatype RE (Eps)
                    (Empty)
                    (Symb [c : Char])
                    (Cat [l : RE] [r : RE] )
                    (Alt [l : RE] [r : RE])
                    (Kle [e : RE])
  )

(define (First [e : RE]) : (Listof Char)
        (match e
          [(Eps) '()] ; Cuidado para representar nulo
          [(Empty) '()]
          [(Symb c) (list c)]
          [(Cat e d) (let [(fe (set-union (First e)))]
                          (cond
                            [(member #\nul fe) (set-union fe (First d))]
                            [else fe])) ]
          [(Alt e d) (set-union (First e) (First d))]
          [(Kle e) (set-union (First e) '() #;(list #\nul))])
  )


(define (delta [e : RE]) : RE
        (match e
          [(Eps)     (Eps)]
          [(Empty)   (Empty)]
          [(Symb c)  (Empty) ]
          [(Cat e d)  (cat (delta e) (delta d))] ; Match and test
          [(Alt e d)  (alt (delta e) (delta d))]
          [(Kle e) (Eps)])
  )

(define (cat [l : RE] [r : RE]) : RE
     (match (cons l r)
        [(cons (Empty) r) (Empty)]
        [(cons l (Empty)) (Empty)]
        [(cons e (Eps))  e]
        [(cons (Eps) l ) l]
        [(cons l r) (Cat l r)])
  )

(define (alt [l : RE] [r : RE]) : RE
     (match (cons l r)
        [(cons (Empty) r) r]
        [(cons l (Empty)) l]
        [(cons l r) (Alt l r)])
  )

(define (kle [e : RE] ) : RE
     (match e
        [(Empty)  (Empty)]
        [x (Kle x)])
  )


(define (derivate [x : Char] [e : RE]) : RE
        (match e
          [(Eps)    (Empty)]
          [(Empty)  (Empty)]
          [(Symb c) (cond [(eq? x c) (Eps)]
                          [else (Empty)] ) ]
          [(Cat e d) (alt (cat (delta e) (derivate x d)) (cat (derivate x e) d)) ]
          [(Alt e d) (alt (derivate x e) (derivate x d))]
          [(Kle e)   (cat (derivate x e) (kle e) )])
  )


(define (pick [l : (Listof Char)] ) : Char
    (list-ref l (random (length l)) )
  )

(define (nullable [e : RE]  ) : Boolean
    (match e
        [(Empty) #f]
        [(Eps) #t]
        [(Symb _) #f]
        [(Alt e r) (or (nullable e) (nullable r) )]
        [(Cat e r) (and (nullable e)  (nullable r) )]
        [(Kle e) #t] )
  )

(define (synth [s : (Listof Char)] [n : Natural] [e : RE] ) : (Listof Char)
   (cond
      [(and (<= n 0) (nullable e)) s]
      [(and (<= n 0) (not (nullable e))) '(#\nul)]
      [(and (> n 0) (> (random 100) 50) (nullable e) ) s]
      [else  (match e
                [(Empty)  s]
                [(Eps)  s]
                [er  (let ([c (pick (First er)) ])
                           (synth (append s (list c)) (max (- n 1) 0) (derivate c er))  )]
        ) ]
       ))

(define (re-gen [e : RE]) : String
  (list->string (synth '() 100 e) )
  )

(define (test [n : Natural] [e : RE]) : Void
      (for ([k (in-range n)])
          (displayln (re-gen e)) 
        )
  )

;(define test (pick (string->list "abcdef")))

(synth '() 100  (Alt (Cat (Symb #\a) (Symb #\c)) (Cat (Kle (Alt (Symb #\a) (Symb #\b)) ) (Symb #\b)  ) ) )