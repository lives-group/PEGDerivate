#lang racket
(require redex)

(define-language Peg
   (p ::= natural
          ϵ
          (seq  p p)
          (/ p p)
          (* p)
          (! p)
          x)
   (x ::= variable-not-ohterwise-mentioned)
   (r ::= (x p) )
   (G ::= (r ...))
   (re ::= Z O F)
  )

(define-relation Peg
  → ⊆ (G p) × re
  [(→ (_ ϵ) Z)]
  [(→ (_ natural) O)]
  [(→ (_ natural) F)]
  [(→ ((r ... (x p) r ...) x) re) (→ ((r ... (x p) r ...) p) re) ]
  [(→ (G (seq p_1 p_2)) Z) (→ (G p_1) Z) (→ (G p_2) Z)]
  [(→ (G (seq p_1 p_2)) O) (→ (G p_1) O) (→ (G p_2) Z)]
  [(→ (G (seq p_1 p_2)) O) (→ (G p_1) O) (→ (G p_2) O)]
  [(→ (G (seq p_1 p_2)) O) (→ (G p_1) Z) (→ (G p_2) O)]
  [(→ (G (seq p_1 p_2)) F) (→ (G p_1) F)]
  [(→ (G (seq p_1 p_2)) F) (→ (G p_1) Z) (→ (G p_2) F)]
  [(→ (G (seq p_1 p_2)) F) (→ (G p_1) O) (→ (G p_2) F)]
  [(→ (G (/ p_1 p_2)) rsuc) (→ (G p_1) rsuc)]
  [(→ (G (/ p_1 p_2)) re) (→ (G p_1) F) (→ (G p_2) re)]
  )