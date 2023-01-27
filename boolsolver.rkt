#lang typed/racket
(require typed-racket-datatype)
(require "./env.rkt")
(require "./opt.rkt")
(provide CExp
         wht
         mkAnd
         mkOr
         mkFalse
         mkTrue
         mkNot
         mkVar
         cexp->string
         value-exp?
         non-value-exp?
         isTrue?
         unsolved-vars
         solve-env
         solve-env-with
         print-cexp-table
         )

(define-datatype CExp
                      (true)
                      (false)
                      (Or  [l : CExp] [r : CExp ] )
                      (And [l : CExp] [r : CExp ] )
                      (Not [l : CExp])
                      (Var [s : String])
  )

(define (wht [e : CExp]) : Natural
    (match e
      [(true)     0]
      [(false)    0]
      [(Var _)    1]
      [(Not c)    (+ 1 (wht c))]
      [(And c1 c2)  (+ 1 (max (wht c1) (wht c2))) ]
      [(Or c1 c2)   (+ 1 (max (wht c1) (wht c2))) ]
      )
  )

(define (parens [b : Boolean] [s : String]) : String
     (match b
           [#f   s]
           [else  (string-append "(" s ")")]
  )
)


(define (cexp-prec [e : CExp] ) : Natural
    (match e
      [(true)     4]
      [(false)    4]
      [(Var _)    4]
      [(Not _)    3]
      [(And _ _)  2]
      [(Or _ _)   1]
      )
  )

; true, false 4   
; Not         3
; And         2  Esq (a & b & c) = (And (And a b) c)
; Or          1  Esq
;
(define (cexp->string [e : CExp]  ) : String
    (match e
      [(true) "true"]
      [(false) "false"]
      [(Var s) s]
      [(And e d) (string-append (parens (< (cexp-prec e) 2)  (cexp->string e))
                                  " & "
                                  (parens (<= (cexp-prec d) 2)  (cexp->string d)))]
      [(Or e d) (string-append (parens (< (cexp-prec e) 2)  (cexp->string e))
                                 " | "
                                (parens (<= (cexp-prec d) 2)  (cexp->string d)))]
      [(Not e)  (string-append "!" (parens (< (cexp-prec e) 3) (cexp->string e)) )   ]
      )
  )

(define (mkAnd [l : CExp] [r : CExp] )
    (match l
      [(true) r]
      [(false) (false)]
      [e1      (cond
                 [(true? r) e1]
                 [(false? r) (false)]
                 [(equal? e1 r) e1]
                 [else (And l r)])])
)


(define (mkOr [l : CExp] [r : CExp])
    (match l
      [(true) (true)]
      [(false) r]
      [e1      (cond
                 [(true? r) (true)]
                 [(false? r) e1]
                 [(equal? e1 r) e1]
                 [else (Or l r)])])
  )

(define (mkNot [l : CExp])
    (match l
      [(true)  (false)]
      [(false) (true)]
      [(Not e) e]
      [e1      (Not e1)])
)

(define (mkVar [s : String]) : CExp
  (Var s)
  )

(define mkTrue : CExp
  (true)
  )

(define mkFalse : CExp
   (false)
  )


(define (isTrue? [e : CExp]) : Boolean
     (true? e)
  )

(define (value-exp? [e : CExp ]) : Boolean
      ( or (true? e) (false? e))
  )

(define (non-value-exp? [e : CExp ]) : Boolean
      (not (or (true? e) (false? e)))
  )


(define (unsolved-vars [env : (ListEnv CExp)] ) : (Listof String)
   (env-pred-keys non-value-exp? env)
  )

(define (smart-lookup [env : (ListEnv CExp)] [v : String] ) : CExp
     (opt (lambda ([x : CExp]) (cond [(value-exp? x) x] [else (Var v)] ))
          (lkp env v)
          (false))
  )

(define (solve-exp [env : (ListEnv CExp)] [envName : String] [c : CExp]) : CExp
     (match c
       [(true) (true)]
       [(false) (false)]
       [(Var s) (cond
                  [(string=? envName s) (Var s)]
                  [else (smart-lookup env s)])]
       [(And e d) (mkAnd (solve-exp env envName e) (solve-exp env envName d))]
       [(Or e d) (mkOr (solve-exp env envName e) (solve-exp env envName d))]
       [(Not e) (mkNot (solve-exp env envName e))]
       )
  )

(define (step-solve-env [env : (ListEnv CExp)]) : (ListEnv CExp)
     (map (lambda ( [x : (Pair String CExp)] ) (cons (car x ) (solve-exp env (car x) (cdr x))) ) env)
  )

(define (solve-env [env : (ListEnv CExp)]) : (ListEnv CExp)
     (let* ([whs : (Listof Natural) (env-map-vals wht env)]
            [env1 : (ListEnv CExp) (step-solve-env env)]
            [whs1 : (Listof Natural) (env-map-vals wht env1)])
           
            (cond
              [(equal? whs whs1) env1]
              [else (solve-env env1)]))
  )

(define (solve-env-with [e : CExp] [env : (ListEnv CExp)]) : (ListEnv CExp)
     (let* ([env1 : (ListEnv CExp) (solve-env env)]
            [unsol : (Listof String) (unsolved-vars env1)])
            (foldr  (lambda ([k : String] [l : (ListEnv CExp) ] ) (env-set l k e) ) env1 unsol)
  )
)

(define (debug-solve-env [fuel : Natural] [env : (ListEnv CExp)]) : (ListEnv CExp)
     (define x : Char #\c)
     (let* ([whs : (Listof Natural) (env-map-vals wht env)]
            [env1 : (ListEnv CExp) (step-solve-env env)]
            [whs1 : (Listof Natural) (env-map-vals wht env1)])
          
            (cond
                [(equal? whs whs1) env1]
                [(> fuel 0) (begin
                                   (println (string-append "-------------" (number->string fuel) " ------------- "))
                                   (print-cexp-table env)
                                   (println (string-append "------------- xxx ------------- "))
                                   (debug-solve-env (- fuel 1) env1))]
                [else env1]
                                                     
            )
     )
 )

(define (print-cexp-table [env : (ListEnv CExp)])
   (for ([x  env])
       (displayln (string-append (car x)  " : " (cexp->string (cdr x))))
   )
 )


(define testEnv : (ListEnv CExp)
  (list (cons "A" (Or (Var "C") (Var "B")))
        (cons "B" (And (Var "B") (Var "A")) )
        (cons "C" (Or (true) (Var "C")))
         ))

(define testEnv2 : (ListEnv CExp)
  (list (cons "A" (And (true) (Var "B")))
        (cons "B" (Or (false) (Var "C")))
        (cons "C" (Not (Var "A")) )
         ))

(define testEnv3 : (ListEnv CExp)
  (list (cons "A"  (Var "B"))
        (cons "B"  (And (Var "C") (Var "B")))
        (cons "C"  (Not (Var "A")) )
         ))

(define testEnv4 : (ListEnv CExp)
  (list (cons "A"  (And (And (Var "A") (Var "B")) (Var "C")))
        (cons "B"  (Or (Var "C") (Var "A")))
        (cons "C"  (And (Not (Var "A")) (false)) )
         ))


(define testEnv5 : (ListEnv CExp)
  (list (cons "A"  (Not (Var "B")))
        (cons "B"  (Var "A"))
        (cons "C"  (false))
         ))