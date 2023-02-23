#lang racket
(require redex)
(require "../pegd-syntax.rkt")
(require "../pegd-syntax.rkt")
(require "../env.rkt")
(require "../pegd-samples.rkt")
(require "../opt.rkt")

(provide (all-defined-out))

(define-language VMPeg
    (State ::= (Ci Cs Stk Cl) ) ; ci = current instruction, cs = current subject
    (Ci ::= N
            Fail)
    (Cs ::= N)
    (Stk ::= (S ...) )
    (S ::= N
           (N N Cl))
    (Cl ::= (Cp ...))
    (Cp :: (N N))
    (Prog ::=  (I ... ))
    (I ::= (Char N)
           (Jump LB)
           (Choice LB)
           (Call LB)
           Return
           (Commit LB)
           (Capture N)
           Fail
           End
           )
    (N ::= natural)
    (LB ::= integer)
    (Str ::= (Chr ...))
    (Chr ::= N EOF)
    (IC := (I Chr State Prog Str))
  )

(define-metafunction VMPeg
   at : (Chr ...) N -> Chr
   [(at () N) EOF]
   [(at (N_1 N ...) 0) N_1]
   [(at (N_1 N ...) N_2) (at (N ...) ,(- (term N_2) 1))]
  )

(define-metafunction VMPeg
   atI : (I ...) N -> I
   [(atI (I_1 I ...) 0) I_1]
   [(atI (I_1 I ...) N_1) (atI (I ...) ,(- (term N_1) 1))]
  )

(define-metafunction VMPeg
   nxtc : Str N -> N  
   [(nxtc Str N) (at Str ,(+ (term N) 1))]
  )

(define-metafunction VMPeg
   nxtI : Prog N -> N  
   [(nxtI Prog N) (atI Prog ,(+ (term N) 1))]
  )

(define vm-red
  (reduction-relation VMPeg
    #:domain IC
    (--> ((Char N)       N            (N_3 Cs Stk Cl)  Prog Str)
         ((atI Prog N_1) (at Str N_2) (N_1 N_2 Stk Cl) Prog Str)
         (where N_1 ,(+ (term N_3) 1))
         (where N_2 ,(+ (term Cs) 1))
         "Ch-Suc"
     )

     (--> ((Char N_1) Chr (N_3  Cs Stk Cl) Prog Str)
         ( (Char N_1) Chr (Fail Cs Stk Cl) Prog Str)
         (side-condition (not (equal? (term N_1) (term Chr))))
         "Ch-Fail"
     )

     (--> ((Jump LB_1)    Chr (N_3 Cs Stk Cl) Prog Str)
          ((atI Prog N_2) Chr (N_2 Cs Stk Cl) Prog Str)
          (where N_2 ,(max 0 (+ (term N_3) (term LB_1))))
          "Jmp"
     )
     
     (--> ((Choice LB)      Chr (N_i   Cs              (S ...) Cl) Prog Str)
          ((atI Prog N_nxi) Chr (N_nxi Cs ((N_lb Cs Cl) S ...) Cl) Prog Str)
          (where N_nxi ,(+ (term N_i) 1))
          (where N_lb ,(max 0 (+ (term N_i) (term LB))))
          "Choice"
     )

     (--> ((Call LB)       Chr (N_i  Cs (S ...)       Cl) Prog Str)
          ((atI Prog N_lb) Chr (N_lb Cs (N_nxi S ...) Cl) Prog Str)
          (where N_nxi ,(+ (term N_i) 1))
          (where N_lb  ,(max 0 (+ (term N_i) (term LB))))
          "Call"
     )
     
     (--> (Return         Chr (N_i  Cs (N_r S ...)  Cl) Prog Str)
          ((atI Prog N_r) Chr (N_r Cs (S ...)      Cl) Prog Str)
          "Ret"
     )

     (--> ((Commit LB)      Chr (N_i   Cs  ((N_ia N_ca Cl_1) S ...)  Cl) Prog Str)
          ((atI Prog N_nxi) Chr (N_nxi Cs  (S ...)             Cl) Prog Str)
          (where N_nxi ,(max 0 (+ (term N_i) (term LB))) )
          "Commit"
     )   

     (--> ((Capture N)      Chr (N_i   Cs Stk (Cp ...)          ) Prog Str)
          ((atI Prog N_nxi) Chr (N_nxi Cs Stk ((N_i Cs) Cp ...) ) Prog Str)
          (where N_nxi ,(+ (term N_i) 1))
          "Capture"
     )

     (--> (Fail Chr (N_i Cs Stk Cl)  Prog Str)
          (Fail Chr (Fail Cs Stk Cl) Prog Str)
          "Fail"
     )   
     
     (--> (_ _ (Fail _ ((N_lb Cs Cl_1) S ...) Cl) Prog Str)
          ((atI Prog N_lb) (at Str Cs) (N_lb Cs ( S ...) Cl_1) Prog Str)
          "Back"
     )

     (--> (I Chr (Fail Cs (N_1 S ...) Cl) Prog Str)
          (I Chr (Fail Cs (S ...)     Cl) Prog Str)
          "Back-drop"
     )
  )
)

(struct CompResult
       (code env)
       #:transparent )

(define (compile-aux chr->num pegd)
    (match pegd
         [(p∅)           (list 'Fail)]
         [(p?)            (list 'Fail)]
         [(pϵ)            (list)]
         [(pSym c)        (list (list 'Char (chr->num c)))]
         [(pVar s)        (list `(Opencal ,s))]
         [(pCat p1 p2)    (append (compile-aux chr->num p1) (compile-aux chr->num p2) )]
         [(pAlt p1 p2)    (let* ([le (compile-aux chr->num p1)]
                                 [ld (compile-aux chr->num p2)]
                                 [k1  (length le) ]
                                 [k2  (length ld)])
                                (append  (list `(Choice ,(+ k1 2))) le (list `(Commit ,(+ k2 1))) ld))
                          ]
         [(pKle p)       (let* ([lp (compile-aux chr->num p)]
                                [k1  (length lp) ])
                              (append  (list `(Choice ,(+ k1 2))) lp (list `(Commit ,(- -1 k1))) )) ]
         
         [(pNot p)       (let* ([lp (compile-aux chr->num p)]
                                [k  (length lp) ])
                              (append  (list `(Choice ,(+ k 3))) lp (list `(Commit 1) `Fail ) )) ]
         ;[(DP c p)        #t]
         ;[(δP c p)        #t]
     ) 
  )



(define (numberAssoc n l)
   (cond
     [(null? l) null]
     [else  (cons (cons (car l) n) (numberAssoc (+ n 1) (cdr l) ))] )
  
 )
  
(define (compile-non-terminals chr->num dpeg)
            (cons (compile-aux chr->num (DPEG-ds dpeg))
                  (env-map (lambda (e) (list (append (compile-aux chr->num e) (list 'Return)))) (DPEG-dv dpeg)) ) 
  )


(define (accum-env-sz acc l)
   (match l
     ['()  null]
     [(cons (cons var sz) xs) (cons (cons var acc) (accum-env-sz (+ acc sz) xs))])
  )

(define (rename-call n nt-map l)
   (match l
     ['()  null]
     [(cons (list 'Opencal s) xs) (let* ([opt-r (lkp nt-map s)]
                                         [addr (- (Some-val opt-r) n)])
                                        (cons (list 'Call addr) (rename-call (+ n 1) nt-map xs)))]
     [(cons x xs) (cons x (rename-call (+ n 1) nt-map xs) )]
   )
  )

(define (link-edit prog-env)
      (match prog-env
         [(cons prog env) (let* ([envsz (env-map (lambda (l) (length (car l))) env)]
                                 [progs (foldr (lambda (a b) (append (car a) b)) null (env-vals env))]
                                 [full-prog (append prog (list 'End) progs )])
                                (rename-call 0 (accum-env-sz (+ (length prog) 1) envsz) full-prog)) ])
  )


(define (compile-vm dpeg)
           (link-edit (compile-non-terminals char->integer dpeg))
      ) 

;(compile-aux (make-hash (list (cons #\a 0) (cons #\b 1) (cons #\d 3)) ) null (pCat (pAlt (pSym #\a) (pSym #\b)) (pSym #\d)))
;(compile-aux (make-hash (list (cons #\a 0) (cons #\b 1) (cons #\d 3)) ) null (pKle (pSym #\a)) )
;(traces vm-red (term ((Choice 2) 0 (0 0  () ()) ((Choice 2) (Char 0) (Char 0)) (0 0 0 1)) ))


(define (input-head l)
    (cond
      [(null? l) 'EOF]
      [else (car l)])
  )

(define (trace-vm l s)
      (traces vm-red (term (,(car l) ,(input-head  s) (0 0 () ()) ,l ,s)) )
  )

(define (run-vm l s)
      (apply-reduction-relation* vm-red (term (,(car l) ,(input-head  s) (0 0 () ()) ,l ,s)) )
  )

(define (state-accpet? l)
       (match l
           [(list i chr (list 'Fail _ _ _) prog inp) #f]
           [(list 'End chr state prog inp)           #t]
           [ _  #f]
        )
  )

(define (accpeted-prefix l)
       (match l
           [(list i chr (list 'Fail _ _ _) prog inp) #f]
           [(list 'End chr (list i p _ _) prog inp) (map integer->char (take inp p))]
           [ _  #f]
        )
  ) 

(define (compile-and-trace-vm peg str)
      (let* ([code (compile-vm peg)]
             [chrl (map char->integer (string->list str))])
            (trace-vm  code chrl)
        )
  )

(define (compile-and-run-vm peg str)
      (let* ([code (compile-vm peg)]
             [chrl (map char->integer (string->list str))])
            (run-vm code chrl)
        )
  ) 


(define (vm-compile-accept? peg str)
      (state-accpet? (car (compile-and-run-vm peg str)) )
  )

(define (vm-compile-accepted-prefix peg str)
      (accpeted-prefix (car (compile-and-run-vm peg str)) )
  )
