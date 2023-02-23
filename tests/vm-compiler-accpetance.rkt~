#lang typed/racket
(require "../pegd-syntax.rkt")
(require "../pegd-derivate.rkt")
(require "../pegd-input-gen.rkt")
(require "../env.rkt")
(require "../ftable.rkt")

(require/typed rackcheck
               [#:opaque G gen?]
               [sample (-> G Natural (Listof Any))]
               [gen:natural G])
(require/typed peg-gen
               [gen:peg (-> Natural Natural Natural G)])


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

(define (import-gen-exp [l : Any]): DPE
   (match l
     [(list '• ee ed) (pCat (import-gen-exp ee) (import-gen-exp ed))]
     [(list '/ ee ed) (pAlt (import-gen-exp ee) (import-gen-exp ed))]
     [(list '* ee) (pKle (import-gen-exp ee))]
     [(list '! ee) (pNot (import-gen-exp ee))]
     ['ϵ  (pϵ)]
     ['∅ (p∅)]
     [n (cond
          [ (number? n) (pSym #\0)]
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