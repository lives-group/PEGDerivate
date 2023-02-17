#lang typed/racket
(require)
(require "./pegd-syntax.rkt")
(require "./pegd-derivate.rkt")
(require "./opt.rkt")
(require "./env.rkt")
(require)
(require "./ftable.rkt")

(provide (all-defined-out))

(define-type DPair (Pair (Listof Char) DPE))

(define (possible-str [xs : (Listof Char)] [d : DPair]) : (Listof DPair)
    (cond
      [(null? xs) null]
      [else (cons (cons (cons (car xs) (car d))
                        (cdr d))
                  (possible-str (cdr xs) d)) ] )
  )

(define (cat-sol [x : Char] [l : (Opt (Listof Char))]) : (Opt (Listof Char))
     (match l
       [(None)    (Some (list x))]
       [(Some xs) (Some (cons x xs))])
  )

(define  (one-of [xs : (Listof Char)]): Char 
      (cond
        [(null? xs) #\000]
        [else       (list-ref xs  (random (length xs)) )])
 )

(define (peg-syn [n : Natural] [Σ : (Listof Char)] [ft : FTable ] [g : DPEG] ) : (Opt (Listof Char))
     (let* ([fs : (Listof Char) (first Σ ft (DPEG-ds g))]
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

(define (derivate-pair [c : Char] [v : (ListEnv DPE)] [d : DPair]) : DPair
   (cons (cons c (car d)) (derivate-dpe c v (cdr d)))
  )

(define (peg-bulk-syn [Σ : (Listof Char)] [ft : FTable ] [v : (ListEnv DPE)] [d : DPair] ) : (Listof DPair)
     (let ([fs : (Listof Char) (first Σ ft (cdr d))])
          (map (lambda ([c : Char]) (derivate-pair c v d)) fs)
     )
  )

(define (peg-input-syn [g : DPEG] ) : (Opt (Listof Char))
   (peg-syn 10 (alphabet-from-grammar g) (dpeg-first g) g)
  )

