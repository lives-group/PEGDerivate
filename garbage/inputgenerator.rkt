#lang typed/racket
(require typed-racket-datatype)
(require "./PEGD2.rkt")
(require "./PEG2Derivate.rkt")
(require "./ftable.rkt")

(provide (all-defined-out))


; Start the pseud random generator
(current-pseudo-random-generator  (make-pseudo-random-generator)) 

(define (gen-input [fuel : Natural] [ft : FTable]  [g : DPEG] [revInput : (Listof Char)] )
      (cond
         [(<= fuel 0) (cond
                        [(dpe-null? (DPEG-dv g) (DPEG-ds g)) (reverse revInput)]
                        [else null])]
         [else (firs ]
      )
)

; S <- ab
; S
(define pex0 : PEG
    (PEG (list (cons "S"  (Cat (Sym #\a) (Sym #\b) )  ))
         (Var "S")) 
  )

; S <- aS / Eps
; S
(define pex1 : PEG
    (PEG (list (cons "S" (Alt (Cat (Sym #\a) (Var "S") ) (Eps) )))
         (Var "S")) 
  )

; S <- aSb / ϵ
; S
(define pex2 : PEG
    (PEG (list (cons "S" (Alt (Cat (Sym #\a) (Cat (Var "S") (Sym #\b))) (Eps))))
         (Var "S")) 
  )

; S <- a*
; S
(define pex3 : PEG
    (PEG (list (cons "S" (Kle (Sym #\a))) )
         (Var "S")) 
  )

; A <- aA \ ϵ
; B <- bA \ c
; ABc
(define pex4 : PEG
    (PEG (list (cons "A" (Alt (Cat (Sym #\a) (Var "A")) (Eps) ))
               (cons "B" (Alt (Cat (Sym #\b) (Var "B")) (Sym #\c) ))
               )
         (Cat (Var "A") (Sym #\b ))) 
  )

; A <- bb*
; a*b*
(define pex5 : PEG
    (PEG (list (cons "A" (Cat (Sym #\b) ( Kle (Sym #\b)) )))
         (Cat (Kle (Sym #\a)) (Kle (Sym #\b)) )) 
  )