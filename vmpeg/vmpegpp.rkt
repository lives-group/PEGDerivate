#lang racket
(require redex)
(require "./vmpeg.rkt")

(define-extended-language VMPPrint VMPeg
     (PState ::= (Ci Cs PStk PStk))
     (PStk ::= (PS ...) )
     (PS ::= N
            (N N))
     (PStr ::= (Pat ...))
     (Pat ::= w N (¬ N) )
     (PSt := (I Pat PState Prog PStr)) 
  ) 

(define-metafunction VMPPrint
   rstStr : PStr N -> PStr  
   [(rstStr () N)   ()]
   [(rstStr (Pat Pat_1 ...) 0) (cons w (rstStr (Pat_1 ...) 0)) ]
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
   [(wrtStr ()                 0 Pat_w)  Pat_w]
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

(define vm-red-pp
  (reduction-relation VMPPrint
    #:domain PSt
    (--> ((Char N)       w                   (N_3 Cs  PStk_1 PStk_2) Prog PStr)
         ((atI Prog N_1) (rdStr PStr_nw N_2) (N_1 N_2 PStk_1 PStk_2) Prog PStr_nw)
         (where N_1 ,(+ (term N_3) 1))
         (where N_2 ,(+ (term Cs) 1))
         (where PStr_nw (wrtStr PStr Cs N))
         "Ch-wrt"
     )

     (--> ((Char N)        N               (N_3 Cs PStk_1 PStk_2)  Prog PStr)
          ((atI Prog N_1) (rdStr PStr N_2) (N_1 N_2 PStk_1 PStk_2) Prog PStr)
          (where N_1 ,(+ (term N_3) 1))
          (where N_2 ,(+ (term Cs) 1))
          "Ch-match"
     )

     (--> ((Char N)     (¬ N_n)            (N_3 Cs PStk_1 PStk_2)  Prog PStr)
          ((atI Prog N_1) (rdStr PStr N_2) (N_1 N_2 PStk_1 PStk_2) Prog PStr)
          (side-condition (not (equal? (term N_1) (term N_n))))
          (where N_1 ,(+ (term N_3) 1))
          (where N_2 ,(+ (term Cs) 1))
          "Ch-not-match"
     )

     (--> ((Char N_1) Pat (N_3  Cs PStk_1 PStk_2) Prog PStr)
         ( (Char N_1) Pat (Fail Cs PStk_1 PStk_2) Prog PStr)
         (side-condition (and (not (equal? (term N_1) (term Pat)))
                              (not (equal? (term Pat) 'w))
                          )
          )
         "Ch-Fail"
     )

     (--> ((Jump LB_1)    Pat (N_3 Cs PStk_1 PStk_2) Prog PStr)
          ((atI Prog N_2) Pat (N_2 Cs PStk_1 PStk_2) Prog PStr)
          (where N_2 ,(max 0 (+ (term N_3) (term LB_1))))
          "Jmp"
     )
     
     (--> ((Choice LB)      Pat (N_i   Cs (PS ...)           PStk_2) Prog PStr)
          ((atI Prog N_nxi) Pat (N_nxi Cs ((N_lb Cs) PS ...) PStk_2) Prog PStr)
          (where N_nxi ,(+ (term N_i) 1))
          (where N_lb ,(max 0 (+ (term N_i) (term LB))))
          "Choice"
     )

     (--> ((Call LB)       Pat (N_i  Cs (PS ...)       PStk) Prog PStr)
          ((atI Prog N_lb) Pat (N_lb Cs (N_nxi PS ...) PStk) Prog PStr)
          (where N_nxi ,(+ (term N_i) 1))
          (where N_lb  ,(max 0 (+ (term N_i) (term LB))))
          "Call"
     )
     
     (--> (Return         Pat (N_i Cs (N_r PS ...)  PStk) Prog Str)
          ((atI Prog N_r) Pat (N_r Cs (PS ...)      PStk) Prog Str)
          "Ret"
     )

    (--> ((Commit LB)       Pat (N_i   Cs  ((N_ia N_ca) PS ...)  (PS_2 ...)) Prog PStr)
          ((atI Prog N_nxi) Pat (N_nxi Cs  (PS ...)  ((N_ia N_ca) PS_2 ...)) Prog PStr)
          (where N_nxi ,(max 0 (+ (term N_i) (term LB))) )
          "Commit"
     )   

     #;(--> ((Capture N)      Chr (N_i   Cs Stk (Cp ...)          ) Prog Str)
          ((atI Prog N_nxi) Chr (N_nxi Cs Stk ((N_i Cs) Cp ...) ) Prog Str)
          (where N_nxi ,(+ (term N_i) 1))
          "Capture"
     )

     (--> (Fail Pat (N_i  Cs PStk ()) Prog PStr)
          (Fail Pat (Fail Cs PStk ()) Prog PStr)
          "Fail"
     )

     (--> (Fail             Pat (N_i   Cs    PStk ((N_ibk Cs_bk) PS ... )) Prog PStr)
          ((atI Prog N_ibk) Pat (N_ibk Cs_bk PStk ((N_ibk Cs_bk) PS ... )) Prog (revStr PStr Cs_bk))
          "Fail-rev"
     )

     
     #;(--> (_ _ (Fail _ ((N_lb Cs Cl_1) S ...) Cl) Prog Str)
          ((atI Prog N_lb) (at Str Cs) (N_lb Cs ( S ...) Cl_1) Prog Str)
          "Back"
     )

     #;(--> (I Chr (Fail Cs (N_1 S ...) Cl) Prog Str)
          (I Chr (Fail Cs (S ...)     Cl) Prog Str)
          "Back-drop"
     )
  )
)
;w w
;a w
;      (¬ a) w
;b w   (¬ a, b)
;(¬ a ¬ b) w 