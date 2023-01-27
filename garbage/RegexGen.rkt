#lang racket
(require redex)

(define-language ER
  (E ::= ϵ
         a
         (Cat E E) 
         (/ E E)
         (* E))
   (a ::= natural)
   (s ::= a ϵ)
   (FSet ::= (s ...) )
  )

(define-relation ER
    nullable ⊆ E
    [(nullable ϵ)]
    [(nullable (Cat E_1 E_2) ) (nullable E_1) (nullable E_2)]
    [(nullable (/ E_1 E_2) ) (nullable E_1)]
    [(nullable (/ E_1 E_2) ) (nullable E_2)]
    [(nullable (* E)) ])

(define-judgment-form ER
  #:mode (First I O) 
  #:contract (First E FSet)
  [------------------"Empty"
    (First ϵ (ϵ))
  ]
  [------------------"Symb"
    (First a (a))
  ]
  [(side-condition (no (nullable E_1)))
   (First E_1 FSet_1)
   ------------------"Cat-no-null"
    (First (Cat E_1 E_2) FSet_1)
  ]
  [(nullable E_1)
   (First E_1 FSet_1)
   (First E_2 FSet_2)
   ------------------"Cat-null"
    (First (Cat E_1 E_2) (∪ FSet_1 FSet_2))
  ]
  
  [(First E_1 FSet_1)
   (First E_2 FSet_2)
   ------------------"Alt"
    (First (/ E_1 E_2) (∪ FSet_1 FSet_2))
  ]
  [
    (First E_1 FSet_1)
   ------------------"Seq"
    (First (* E_1) (∪ FSet_1 (ϵ)))
  ]
  )

(define-metafunction ER
   no : boolean -> boolean
   [(no #t) #f]
   [(no #f) #t]
  )

(define-metafunction ER
   ∪ : FSet FSet -> FSet
   [(∪ () FSet) FSet]
   [(∪ FSet ()) FSet]
   [(∪ (s_0 ... s_1 s_3 ...) (s_1 s_4 ...)) (∪ (s_0 ... s_1 s_3 ...) (s_4 ...))]
   [(∪ (s_0 s_2 ...) (s_1 s_3 ...)) (∪ (s_1 s_0 s_2 ...) (s_3 ...))]
  )