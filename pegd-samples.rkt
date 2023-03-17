
#lang typed/racket
(require "./pegd-syntax.rkt")
(require "./pegd-derivate.rkt")
(require "./pegd-fderivate.rkt")
(require "./pegd-input-gen.rkt")

(provide  (all-defined-out))

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

; A<- ab*
; A
(define pex5 : DPEG
    (DPEG (list (cons "A" (pCat ( pKle (pSym #\a)) (pSym #\b))))
         (pVar "A")) 
  )


; A <- aA / ϵ
; B <- AB / BA
; AB
;(loop)
(define pex6 : DPEG
    (DPEG (list (cons "A" (pAlt (pCat (pSym #\a) (pVar "A") )  (pϵ) ))
               (cons "B" (pAlt (pCat (pVar "A") (pVar "B")) (pCat (pVar "B") (pVar "A")))) )
         (pCat (pVar "A") (pVar "B") )) 
  )



; Ford's wrong An Bn Cn
; S <- !!A a* B 
; A <- aAb / eps
; B <- bBc / eps
; Extended regular expressions !

(define pex7 : DPEG
    (DPEG (list (cons "A" (pAlt (pCat (pCat (pSym #\a) (pVar "A") ) (pSym #\b))  (pϵ) ))
                (cons "B" (pAlt (pCat (pCat (pSym #\b) (pVar "B") ) (pSym #\c))  (pϵ) )) )
         (pCat (pCat (pNot (pNot (pVar "A"))) (pKle (pSym #\a))) (pVar "B") )) 
  )


; S <- !(ab)c
(define pex8 : DPEG
    (DPEG (list (cons "S" (pCat (pNot (pCat (pSym #\a) (pSym #\b))) (pSym #\c) )))
          (pVar "S")) 
  )


; S <- !(ab*c)c
(define pex9 : DPEG
    (DPEG (list (cons "S" (pCat (pNot (pCat (pSym #\a) (pSym #\b))) (pSym #\c) )))
          (pVar "S")) 
  )

;A <- aAb / e
;B <- bB  / b
;C <- c
; A B C
(define pex10 : DPEG
    (DPEG (list (cons "A" (pAlt (pCat  (pCat (pSym #\a) (pVar "A")) (pSym #\b) ) (pSym #\e) ) )
                (cons "B" (pAlt (pCat (pSym #\b) (pVar "B")) (pSym #\b)))
                (cons "C" (pSym #\c) )
           )
          (pCat (pCat (pVar "A") (pVar "B")) (pVar "C"))) 
  )


(define pex11 : DPEG
    (DPEG '()
          (pNot (pCat (pSym #\a) (pSym #\b)) )) 
  )

(define pex12 : DPEG
    (DPEG '()
          (pNot (pNot (pCat (pSym #\a) (pSym #\b)) )) ) 
  )

(define pex13 : DPEG
    (DPEG '()
          (pCat (pNot (pCat (pSym #\a) (pSym #\b)) )
                (pCat (pSym #\a) (pSym #\c) )) 
     ) 
 )

(define pex14 : DPEG
    (DPEG '()
          (pCat (pNot (pSym #\a) )
                (pCat (pSym #\a) (pSym #\c) )) 
     ) 
 )

(define pex15 : DPEG
    (DPEG '()
          (pNot (pϵ)) ) 
  )

(define pex16 : DPEG
    (DPEG '()
           (pCat (pAlt (pSym #\a) (pCat (pSym #\a) (pSym #\b) ) ) (pSym #\c) ))  
  )