#lang racket
(require "../pegd-syntax.rkt")

(require Redex-PEG/peg/lang/peg)

(define (redxPeg->pegd rexPEG)
    0
  )

(define (redxPegExp->pegdExp rexPEG)
    (match rexPEG
       [(list '/ e1 e2) (pAtl (redxPegExp->pegdExp e1) (redxPegExp->pegdExp e2))]
       [(list '• e1 e2) (pCat (redxPegExp->pegdExp e1) (redxPegExp->pegdExp e2))]
       [(list '* e1)    (pKle (redxPegExp->pegdExp e1))]
       [(list '! e1)    (pNot (redxPegExp->pegdExp e1))]
       ['ϵ              (pϵ)]
       []
    
      )
  )

(define (redxPeg->pegd rexPEG)
    0
  )