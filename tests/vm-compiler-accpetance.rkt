#lang racket
(require "../pegd-syntax.rkt")
(require "../pegd-samples.rkt")
(require "../vmpeg/vmpeg.rkt")
(require "../env.rkt")
(require "./pegd-peggen-interface.rkt")
(require Redex-PEG/peg/lang/peg)
(require redex)
(require Redex-PEG/peg/lang/smallStepSemantics)
(require peg-gen)
(require rackcheck)


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

(define (pegSSS-prefix-state st)
     (match st
        [(list g '⊢ '() e '↑ sufix prefix 'suc p-stk) (map nat->pegchar (reverse prefix))]
        [_ #f]
       )
  )

(define (gen:listchar peg maxLen)
      (let ([l (alphabet-from-peg peg)])
           (cond
              [(null? l) (gen:const null) ]
              [else     (gen:list (gen:one-of l) #:max-length maxLen)])
      )
  )



(define-property vm-compile-and-run
  ([peg (gen:peg 2 2 2)]
   [inp (gen:listchar peg 5)] )
  (let* ([pegd (import-gen-peg peg)]
         [str  (list->string (map nat->pegchar inp))])
        (begin (vm-compile-accept? pegd str) #t)
    )
 )

(define-property vm-compile-weak-acc
  ([peg (gen:peg 2 2 2)]
   [inp (gen:listchar peg 5)] )
  (let* ([pegd (import-gen-peg peg)]
         [str  (list->string (map nat->pegchar inp))])
         (equal? (vm-compile-accept? pegd str)
                 (pegSSS-accept? peg inp))
    )
  )

(define-property vm-compile-strong-acc
  ([peg (gen:peg 2 2 2)]
   [inp (gen:listchar peg 5)] )
  (let* ([pegd (import-gen-peg peg)]
         [str  (list->string (map nat->pegchar inp))]
         [prefix-vm  (vm-compile-accepted-prefix pegd str) ]
         [prefix-sss (pegSSS-accepted-prefix peg inp)])
         (equal?  prefix-sss
                  prefix-vm)
    )
  )

#;(check-property
    (make-config #:tests 1000 #:deadline (+ (current-inexact-milliseconds) (* 60 1000)) )
    vm-compile-and-run
 )

; Contra-exemplos gerados pelo Rackcheck
(define fail-peg
   '((A (* 0) ∅) (! A)  (A #t ()))
  )
(define fail-input '(0 0 0 0 0))
  ; Comportamento: Exceção (car ()) 
  ; Diagnóstico: Falha na compilação do NOT 
  
(define fail-peg2
   '(∅ (/ (/ ε 1) (• ε ε)) ())
  )

(define fail-input2 '(1 1 1 1 1))
  ; Comportamento: Exceção atI: no clauses matched for (atI () 1) 
  ; Diagnóstico: Erro na compilação do choice, ao calcuar o salto para o segundo ramos
  ;               usava-se a lista de instrução do primeiro ramo.
  

(define fail-peg3
   '(∅ ε ())
  )
(define fail-input3 '())
  ; Comportamento: VM e Sem Passo Pequeno divergem na aceitação. 
  ; Diagnóstico: Erro na conversão do estado de saída do pegSSS-accpet-state?
  ;              '⊤ foi usado ao invés de 'suc
  

(define fail-peg4
   '(∅ (/ (/ 2 1) (• 1 ε)) ())
  )
(define fail-input4 '())
  ; Comportamento: map: contract violation
  ;                   expected: list?
  ;                   given: #f
  ; Diagnóstico: A Propriedade tentar converter a saída do pegSSS-accepted-prefix para uma list de char.
  ;              No entanto a saída de pegSSS-accepted-prefix pode ser #f se a peg rejeita a entrada. 
  ;              A conversão de numero para char passou a ser feita na função pegSSS-prefix-state 
  ;
