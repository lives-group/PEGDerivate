#lang typed/racket
(require typed-racket-datatype)
(provide (all-defined-out))

(define-type NameEnv (Listof (Pair String a) ))

(define emptyName)
