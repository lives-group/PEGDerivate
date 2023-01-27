#lang info
(define collection "peginputgen")
(define deps '("base" "typed-racket" "rackcheck" "rackunit" "peg-gen"))
(define build-deps '("cover-lib"
                     "typed-peg"
                     "scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/pegd-derivate.scrbl" ())))
(define pkg-desc "Correct input synthesis for well formed PEGs ")
(define version "0.0")
(define pkg-authors '(EltonMC RodrigoGR LeonardoVSR ))
