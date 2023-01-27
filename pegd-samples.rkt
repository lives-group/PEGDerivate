#lang typed/racket
(require "./pegd-syntax.rkt")
(require "./pegd-derivate.rkt")
(require "./pegd-input-gen.rkt")

; S <- ab 
(define pex0 : DPEG
    (DPEG (list (cons "S"  (pCat (pSym #\a) (pSym #\b) )  ))
          (pVar "S")) 
  )

; S <- aS / Eps
(define pex1 : DPEG
    (DPEG (list (cons "S" (pAlt (pCat (pSym #\a) (pVar "S") ) (pϵ) )))
         (pVar "S")) 
  )
; S <- aSb / ϵ
(define pex2 : DPEG
    (DPEG (list (cons "S" (pAlt (pCat (pSym #\a) (pCat (pVar "S") (pSym #\b))) (pϵ))))
         (pVar "S")) 
  )

; S <- a*
(define pex3 : DPEG
    (DPEG (list (cons "S" (pKle (pSym #\a))) )
         (pVar "S")) 
  )

; A <- aA / ϵ
; B <- bB / c
; AB
(define pex4 : DPEG
    (DPEG (list (cons "A" (pAlt (pCat (pSym #\a) (pVar "A")) (pϵ) ))
                (cons "B" (pAlt (pCat (pSym #\b) (pVar "B")) (pSym #\c) ))
               )
         (pCat (pVar "A") (pVar "B" ))) 
  )

; A<- bb*
; a*b*
(define pex5 : DPEG
    (DPEG (list (cons "A" (pCat (pSym #\b) ( pKle (pSym #\b)) )))
         (pCat (pKle (pSym #\a)) (pKle (pSym #\b)) )) 
  )


; A <- aA / ϵ
; B <- AB / BA
; AB
(define pex6 : DPEG
    (DPEG (list (cons "A" (pAlt (pCat (pSym #\a) (pVar "A") )  (pϵ) ))
               (cons "B" (pAlt (pCat (pVar "A") (pVar "B")) (pCat (pVar "B") (pVar "A")))) )
         (pCat (pVar "A") (pVar "B") )) 
  )