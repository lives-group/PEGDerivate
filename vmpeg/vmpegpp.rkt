#lang racket
(require redex)
(require "./vmpeg.rkt")
(require "../pegd-samples.rkt")

(define-extended-language VMPPrint VMPeg
     (PState ::= (Ci Cs PStk PStk))
     (PStk ::= (PS ...) )
     (PS ::= N
            (N N))
     (PStr ::= (Pat ...))
     (Pat ::= w N (¬ N) EOF)
     (Fuel ::= (Sc N) Stop)
     (PSt := (I Pat PState Prog PStr Fuel)) 
  ) 


(define-metafunction VMPPrint
   nib : Fuel -> Fuel  
   [(nib Stop)   Stop]
   [(nib (Sc 1)) Stop ]
   [(nib (Sc N)) (Sc ,(- (term N) 1)) ]
  )

(define-metafunction VMPPrint
   refuel : Cs Cs Fuel -> Fuel
   [(refuel Cs Cs Fuel)  Fuel ]
   [(refuel  Cs_i Cs_f (Sc N))  (Sc ,(+ (abs (- (term Cs_f) (term Cs_i)) ) (term N))) ]
   [(refuel  Cs_i Cs_f Stop)    (Sc ,(abs (- (term Cs_f) (term Cs_i)) ))]
  )

(define-metafunction VMPPrint
   rstStr : PStr N -> PStr  
   [(rstStr () N)   ()]
   [(rstStr (Pat Pat_1 ...) 0) (cons w   (rstStr (Pat_1 ...) 0)) ]
   [(rstStr (Pat Pat_1 ...) N) (cons Pat (rstStr (Pat_1 ...) ,(- (term N) 1) ) ) ]
  )

(define-metafunction VMPPrint
   cons : Pat PStr -> PStr  
   [(cons Pat (Pat_2 ...) )   (Pat Pat_2 ...)]
  )

(define-metafunction VMPPrint
   revP : Pat -> Pat  
   [(revP  N)    (¬ N)]
   [(revP (¬ N))  N ]
   [(revP w)      w]
  )

(define-metafunction VMPPrint
   revStr : PStr N -> PStr  
   [(revStr () N)   ()]
   [(revStr (Pat Pat_1 ...) 0) (cons (revP Pat) (revStr (Pat_1 ...) 0)) ]
   [(revStr (Pat Pat_1 ...) N) (cons Pat (revStr (Pat_1 ...) ,(- (term N) 1) ) ) ]
  )


(define-metafunction VMPPrint
   wrtStr : PStr N Pat -> PStr  
   [(wrtStr ()                 0 Pat_w)  (Pat_w)]
   [(wrtStr (Pat_x Pat_xs ...) 0 Pat_w)  (Pat_w Pat_xs ...)]
   [(wrtStr ()                 N Pat_w)  (cons w (wrtStr () ,(- (term N) 1) Pat_w ) )]
   [(wrtStr (Pat_x Pat_xs ...) N Pat_w)  (cons Pat_x (wrtStr (Pat_xs ...) ,(- (term N) 1) Pat_w ) ) ]
  )

(define-metafunction VMPPrint
   rdStr : PStr N -> Pat  
   [(rdStr () N )  w]
   [(rdStr (Pat_x Pat ...)    0)  Pat_x]
   [(rdStr (Pat_x Pat_xs ...) N)  (rdStr (Pat_xs ...) ,(- (term N) 1) ) ]
  )

(define-metafunction VMPPrint
   isRestriction : Pat -> boolean  
   [(isRestriction N )     #true]
   [(isRestriction (¬ N) ) #true]
   [(isRestriction _)      #false]
  )

(define vm-red-pp
  (reduction-relation VMPPrint
    #:domain PSt
    (--> ((Char N)       w                   (N_3 Cs  PStk_1 PStk_2) Prog PStr     (Sc N_f))
         ((atI Prog N_1) (rdStr PStr_nw N_2) (N_1 N_2 PStk_1 PStk_2) Prog PStr_nw  (nib (Sc N_f)))
         (where N_1 ,(+ (term N_3) 1))
         (where N_2 ,(+ (term Cs) 1))
         (where PStr_nw (wrtStr PStr Cs N))
         "Ch-wrt"
     )

     (--> ((Char N)        N               (N_3 Cs PStk_1 PStk_2)  Prog PStr (Sc N_f))
          ((atI Prog N_1) (rdStr PStr N_2) (N_1 N_2 PStk_1 PStk_2) Prog PStr (nib (Sc N_f)))
          (where N_1 ,(+ (term N_3) 1))
          (where N_2 ,(+ (term Cs) 1))
          "Ch-match"
     )

     (--> ((Char N)       (¬ N_n)          (N_3 Cs PStk_1 PStk_2)  Prog (Sc N_f))
          ((atI Prog N_1) (rdStr PStr N_2) (N_1 N_2 PStk_1 PStk_2) Prog PStr_nw (nib (Sc N_f)))
          ;(side-condition (not (equal? (term N) (term N_n))))
          (where N_1 ,(+ (term N_3) 1))
          (where N_2 ,(+ (term Cs) 1))
          (where #true (isRestriction (rdStr PStr N_2)))
          (where PStr_nw (wrtStr PStr Cs N))
          "Ch-not-match"
     )

     (--> ((Char N)       (¬ N_n)          (N_3 Cs PStk_1 PStk_2)  Prog (Sc N_f))
          ((atI Prog N_1) (rdStr PStr N_2) (N_1 N_2 PStk_1 PStk_2) Prog (nib (Sc N_f)))
          (side-condition (not (equal? (term N) (term N_n))))
          (where N_1 ,(+ (term N_3) 1))
          (where N_2 ,(+ (term Cs) 1))
          (where PStr_nw (wrtStr PStr Cs N))
          "Ch-not-sing"
     )

     (--> ((Char N_1) Pat (N_3  Cs PStk_1 PStk_2) Prog PStr Fuel)
         ( (Char N_1) Pat (Fail Cs PStk_1 PStk_2) Prog PStr Fuel)
         (side-condition (or (and (not (equal? (term N_1) (term Pat)))
                              (not (equal? (term Pat) 'w))
                              (equal? (term (¬ N_1)) (term Pat))
                              )
                              (equal? (term Fuel) (term Stop))
                          )
          )
         (where Cs_2 ,(+ (term Cs) 1))
         (where #false (isRestriction (rdStr PStr Cs_2)))
         "Ch-Fail"
     )

     (--> ((Jump LB_1)    Pat (N_3 Cs PStk_1 PStk_2) Prog PStr Fuel)
          ((atI Prog N_2) Pat (N_2 Cs PStk_1 PStk_2) Prog PStr Fuel)
          (where N_2 ,(max 0 (+ (term N_3) (term LB_1))))
          "Jmp"
     )
     
     (--> ((Choice LB)      Pat (N_i   Cs (PS ...)           PStk_2) Prog PStr Fuel)
          ((atI Prog N_nxi) Pat (N_nxi Cs ((N_lb Cs) PS ...) PStk_2) Prog PStr Fuel)
          (where N_nxi ,(+ (term N_i) 1))
          (where N_lb ,(max 0 (+ (term N_i) (term LB))))
          "Choice"
     )

     (--> ((Call LB)       Pat (N_i  Cs (PS ...)       PStk) Prog PStr Fuel)
          ((atI Prog N_lb) Pat (N_lb Cs (N_nxi PS ...) PStk) Prog PStr Fuel)
          (where N_nxi ,(+ (term N_i) 1))
          (where N_lb  ,(max 0 (+ (term N_i) (term LB))))
          "Call"
     )
     
     (--> (Return         Pat (N_i Cs (N_r PS ...)  PStk) Prog PStr Fuel)
          ((atI Prog N_r) Pat (N_r Cs (PS ...)      PStk) Prog PStr Fuel)
          "Ret"
     )

    (--> ((Commit LB)       Pat (N_i   Cs  ((N_ia N_ca) PS ...)  (PS_2 ...)) Prog PStr Fuel)
          ((atI Prog N_nxi) Pat (N_nxi Cs  (PS ...)  ((N_ia N_ca) PS_2 ...)) Prog PStr Fuel)
          (where N_nxi ,(max 0 (+ (term N_i) (term LB))) )
          "Commit"
     )   

     #;(--> ((Capture N)      Chr (N_i   Cs Stk (Cp ...)          ) Prog Str Fuel)
          ((atI Prog N_nxi) Chr (N_nxi Cs Stk ((N_i Cs) Cp ...) ) Prog Str Fuel)
          (where N_nxi ,(+ (term N_i) 1))
          "Capture"
     )

     (--> (Fail Pat (N_i  Cs PStk ()) Prog PStr Fuel)
          (Fail Pat (Fail Cs PStk ()) Prog PStr Fuel)
          "Fail"
     )

     (--> (Fail             Pat (N_i   Cs    PStk ((N_ibk Cs_bk) PS ... )) Prog PStr Fuel)
          ((atI Prog N_ibk) (rdStr PStr_nw Cs_bk) (N_ibk Cs_bk PStk ((N_ibk Cs_bk) PS ... )) Prog PStr_nw Fuel)
          (where PStr_nw (revStr PStr Cs_bk))
          "Fail-rev"
     )

     
     #;(--> (_                _                   (Fail Cs_f  ((N_lb Cs_i) PS ...) PStk_2) Prog PStr    Fuel)
          ((atI Prog N_lb) (rdStr PStr_nw Cs_i) (N_lb Cs_i  ( PS ...)            PStk_2) Prog PStr_nw Fuel_nw)
          (where PStr_nw (rstStr PStr Cs_i))
          (where Fuel_nw (refuel Cs_i Cs_f Fuel))
          "Back"
     )

     (--> (_                _                   (Fail Cs_f  ((N_lb Cs_i) PS ...) PStk_1) Prog PStr    Fuel)
          ((atI Prog N_lb) (rdStr PStr_nw Cs_i) (N_lb Cs_i  ( PS ...)            PStk_1) Prog PStr_nw Fuel_nw)
          (where PStr_nw (rstStr PStr Cs_i))
          (where Fuel_nw (refuel Cs_i Cs_f Fuel))
          "Back-Alt"
     )

     (--> (_                _                   (Fail Cs_f () ((N_lb Cs_i) PS ...)) Prog PStr    Fuel)
          ((atI Prog N_lb) (rdStr PStr_nw Cs_i) (N_lb Cs_i () (PS ...))             Prog PStr_nw Fuel_nw)
          (where PStr_nw (rstStr PStr Cs_i))
          (where Fuel_nw (refuel Cs_i Cs_f Fuel))
          "Back-Alt-empty"
     )
     
     (--> (I Pat (Fail Cs (N_1 PS ...) PStk_2) Prog PStr Fuel)
          (I Pat (Fail Cs (PS ...)     PStk_2) Prog PStr Fuel)
          "Back-drop"
     )
  )
)

(define (run-print-machine code input-pat fuel)
  (apply-reduction-relation* vm-red-pp (term (,(car code) w (0 0 () ()) ,code ,input-pat (Sc ,fuel))) )
  )


(define (trace-print-machine code input-pat fuel)
  (traces vm-red-pp (term (,(car code) w (0 0 () ()) ,code ,input-pat (Sc ,fuel))) )
  )


(define (compile-and-run-ppvm peg pat fuel)
      (let* ([code (compile-vm peg)])
            (run-print-machine code pat fuel)
        )
  )

(define (compile-and-trace-ppvm peg pat fuel)
      (let* ([code (compile-vm peg)])
            (trace-print-machine code pat fuel)
        )
  ) 

;w w
;a w
;      (¬ a) w
;b w   (¬ a, b)
;(¬ a ¬ b) w 