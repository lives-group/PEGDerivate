#lang typed/racket
(require "../pegd-syntax.rkt")
(require "../pegd-derivate.rkt")
(require "../pegd-input-gen.rkt")
(require "../env.rkt")
(require "../ftable.rkt")

(provide (all-defined-out))

(require/typed rackcheck
               [#:opaque G gen?]
               [sample (-> G Natural (Listof Any))]
               [gen:natural G]
               [gen:map  (-> G (-> Any Any) G)])
(require/typed peg-gen
               [gen:peg (-> Natural Natural Natural G)]
               )




(define (gen:pegd [nVar : Natural ] [nTerm : Natural] [depth : Natural]) : G
     (gen:map (gen:peg nVar nTerm depth)  import-gen-peg)
 )
 

(define (import-gen-peg [l : Any]): DPEG
   (match l
     [(list g e ty) (DPEG (import-gen-grm g) (import-gen-exp e))]
     [_  (DPEG null (p∅))]
  )
)

#;
(define (import-gen-with-nulls [l : Any]): (Pair DPEG FTable)
   (match l
     [(list g e ty) (DPEG (import-gen-grm g) (import-gen-exp e))]
     [_  (DPEG null (p∅))]
  )
)


(define (nat->pegchar [n : Natural]) : Char
     (integer->char (+ n 97))
  )

(define (pegchar->nat [c : Char ]) : Natural
     (max 0 (- (char->integer c) 97))
  )

(define (import-gen-exp [l : Any]): DPE
   (match l
     [(list '• ee ed) (pCat (import-gen-exp ee) (import-gen-exp ed)) ]
     [(list '/ ee ed) (pAlt (import-gen-exp ee) (import-gen-exp ed)) ]
     [(list '* ee)    (pKle (import-gen-exp ee)) ]
     [(list '! ee)    (pNot (import-gen-exp ee)) ]
     ['ε  (pϵ)]
     ['∅ (p∅)]
     [n (cond
          [ (natural? n) (pSym (nat->pegchar n)) ]
          [ (symbol? n) (pVar (symbol->string n) )]
          [ else (error "Error while converting from random generated peg")])]
  )
)

(define (import-gen-grm [l : Any]): (ListEnv DPE)
   (match l
     ['∅  null]
     [(list v e g) (cond
                     [(symbol? v) (env-ins (import-gen-grm g) (symbol->string v) (import-gen-exp e) )]
                     [else (error "Error while converting from random generated peg") ])]
  )
)

(define (alphabet-from-pegexp [l : Any]): (Listof Natural)
   (match l
     [(list '• ee ed) (set-union (alphabet-from-pegexp  ee) (alphabet-from-pegexp  ed)) ]
     [(list '/ ee ed) (set-union (alphabet-from-pegexp  ee) (alphabet-from-pegexp  ed)) ]
     [(list '* ee)    (alphabet-from-pegexp ee) ]
     [(list '! ee)    (alphabet-from-pegexp ee) ]
     ['ϵ  null]
     ['∅ null]
     [n (cond
          [ (natural? n) (list n) ]
          [ (symbol? n) null ]
          [ else (error "Error while converting from random generated peg")])]
  )
)

(define (alphabet-from-peg-grm [l : Any]): (Listof Natural)
   (match l
     ['∅  null]
     [(list v e g) (cond
                     [(symbol? v) (set-union (alphabet-from-peg-grm g) (alphabet-from-pegexp e) )]
                     [else (error "Error while converting from random generated peg") ])]
  )
)
(define (alphabet-from-peg [l : Any]): (Listof Natural)
   (match l
     [(list g e ty) (set-union (alphabet-from-peg-grm g) (alphabet-from-pegexp e))]
     [_  null]
  )
)

#;(DPEG '(("A" . #(struct:pCat #(struct:pVar "ε") #(struct:pSym #\c)))) (pAlt (pVar "ε") (pVar "ε")))

