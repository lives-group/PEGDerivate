#lang racket
(require "../pegd-syntax.rkt")
(require "../pegd-samples.rkt")
(require "../pegd-input-gen.rkt")
(require "../env.rkt")
(require "../opt.rkt")
(require "./pegd-peggen-interface.rkt")
(require "./pegsss-interface.rkt")
;(require peg-gen)
(require rackcheck)

(define (syn-str dg)
       (map pegchar->nat
            (opt (lambda (x) x)
                 (peg-input-syn dg)
                 (list)))
  )

(define-property accept-gen
   ([pegg (gen:peg-s 3 2 2 #f)])

   (let* ([dpeg (import-gen-peg pegg)]
          [str (syn-str dpeg)] )
          (begin
             (display pegg)
             (cond
                [(pegSSS-accept? pegg str)  #t]
                [else (begin (display str) #f)])
            )
     )    
  )


(define-property always-gen
   ([pegg (gen:peg-s 3 2 2 #f)])
   (begin
     (displayln  pegg)
     (displayln "----------------------")
     (syn-str (import-gen-peg pegg)))
  )


;(check-property (make-config #:tests 2) accept-gen)
(define gerror '((A (* 0) ∅) (• ε 0) ((A #t ()))))
(define gerror2 '((J (* (• 0 ε)) (L (/ (• ε 0) (• J J)) (E (• (/ 0 L) (* 0)) ∅))) (• (/ 0 0) (• E 0)) ((E #t (J L)) (L #t (J)) (J #t ()))))