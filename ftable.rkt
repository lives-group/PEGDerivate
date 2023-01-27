#lang typed/racket
(require typed-racket-datatype)
(provide FTable
         ft-ins-all
         ft-get
         ft-rst
         mk-empty-ftable
         ft-set-nullable
         ft-changed?
         ft-is-nullable?
         )

(define-datatype FEntry (E [chg : Boolean] [null : Boolean] [ set : (Listof Char)]))
(define-type FTable (Immutable-HashTable String FEntry) )


(define (f-ins [ x : FEntry ] [y : Char]) : FEntry
   (cond
     [(member y (E-set x)) (E  (E-chg x) (E-null x) (E-set x))]
     [else (E #t (E-null x) (cons y (E-set x)) )]
  )
)

(define (f-rst [ x : FEntry ]) : FEntry
     (E #f (E-null x) (E-set x))
)

(define (f-empty) : FEntry
     (E #f #f null)
  )

(define (f-ins-all [ x : FEntry ] [l : (Listof Char)] ) : FEntry
     (foldr (lambda ([c : Char] [z : FEntry]) (f-ins z c) ) x l) 
  )


(define (ft-ins-all [ t : FTable ] [s : String] [l : (Listof Char)] ) : FTable
     (hash-update t s (lambda ([e : FEntry]) (f-ins-all e l)) (lambda () (E #t #f null))) 
  )

(define (ft-get [t : FTable] [s : String] ) : (Listof Char)
  (E-set (hash-ref t s))
)

(define (ft-is-nullable? [t : FTable] [s : String] ) : Boolean
   (E-null (hash-ref t s))
)

(define (ft-set-nullable [t : FTable] [s : String] [b : Boolean] ) : FTable
   (hash-update t s (lambda ([e : FEntry]) (E (E-chg e) b (E-set e))) (lambda () (E #t b null)) )
)


(define (ft-rst [t : FTable] ) : FTable
    (make-immutable-hash (hash-map t (lambda ([k : String] [e : FEntry]) (cons k (f-rst e))) ))
)

(define (ft-changed? [t : FTable] ) : Boolean
    (not (andmap (lambda ([b : Boolean]) (not b))
         (hash-map t (lambda ([k : String] [e : FEntry])  (E-chg e)) )  ))
)

(define (mk-empty-ftable [v : (Listof String)] ) : FTable
      (make-immutable-hash (map (lambda ([ x : String])  (cons x (f-empty) ) ) v) )
  )
