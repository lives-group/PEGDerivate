#lang racket/base

(require "./pegd-syntax.rkt"
         "./pegd-derivate.rkt"
         "./pegd-input-gen.rkt"
         "./opt.rkt"
         "./ftable.rkt"
         "./boolsolver.rkt"
         "./opt.rkt"
         "./env.rkt"
         rackcheck
         rackunit)

(provide (all-from-out "./pegd-input-gen.rkt" ))

(module+ test
   (require rackunit
            rackcheck)
 
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (begin
    (display "Tests no yet being executed")
  ))

(module+ main
    ;(require "peg-gen.rkt")
    ;(provide (all-from-out "peg-gen.rkt" ))  
)
