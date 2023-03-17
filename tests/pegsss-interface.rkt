#lang racket

(require Redex-PEG/peg/lang/peg)
(require redex)
(require Redex-PEG/peg/lang/smallStepSemantics)

(provide (all-defined-out))

(define (pegSSS-accept? gen-peg inp)
     (match gen-peg
        [(list g e t) (pegSSS-accept-state? (car (apply-reduction-relation* red (term (,g ⊢ () ,e ↓ ,inp () ⊥ (0)) ))))  ]
       )
  )

(define (pegSSS-run gen-peg inp)
     (match gen-peg
        [(list g e t)  (car (apply-reduction-relation* red (term (,g ⊢ () ,e ↓ ,inp () ⊥ (0)) )))  ]
       )
  )

(define (pegSSS-accepted-prefix gen-peg inp)
     (match gen-peg
        [(list g e t) (pegSSS-prefix-state (car (apply-reduction-relation* red (term (,g ⊢ () ,e ↓ ,inp () ⊥ (0)) ))))  ]
       )
  )


(define (pegSSS-accept-state? st)
     (match st
        [(list g '⊢ '() e '↑ sufix prefix 'suc p-stk) #t]
        [_ #f]
       )
  )

(define (pegSSS-prefix-state nat->char st)
     (match st
        [(list g '⊢ '() e '↑ sufix prefix 'suc p-stk) (map nat->char (reverse prefix))]
        [_ #f]
       )
  )

