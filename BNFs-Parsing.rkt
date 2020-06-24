#lang pl

#|===========================================================Q.1=============================================================|#
;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))


;; The actual interpreter
#| BNF for the RegE language:
 <ROL> ::= {reg-len = <num> <regE>}
 <RegE> ::= <Bits>
           |{Shl <Bits>}
           |{And <RegE> <RegE>}
           |{Or <RegE> <RegE>}
 <Bits> ::= {(0,1)+}

1) <ROL> => {reg-len = <num> <regE>} => {reg-len = 4 {Shl <Bits>}} => {reg-len = 4 {Shl {1 0 1 0}}
2) <ROL> => {reg-len = <num> <regE>} => {reg-len = 3 {And <RegE> <RedE>}} => {reg-len = 3 {And <Bits> <Bits>}} => {reg-len = 3 {And {1 0 0} {1 0 0}}
3) <ROL> => {reg-len = <num> <regE>} => {reg-len = 5 {Or <RegE> <RedE>}} => {reg-len = 5 {Or {1 1 1 1 1} {And <regE> <RegE>}} => {reg-len = 5 {Or {1 1 1 1 1} {And <Bits> <Bits>}}
 |#


;; RegE abstract syntax trees
(define-type RegE
 [Reg Bit-List]
 [And RegE RegE]
 [Or RegE RegE]
 [Shl RegE])


;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
 (: list->bit-list : (Listof Any) -> Bit-List)
 ;; to cast a list of bits as a bit-list
 (define (list->bit-list lst)
 (cond [(null? lst) null]
       [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
       [else (cons 0 (list->bit-list (rest lst)))]))


 (: parse-sexpr : Sexpr -> RegE)
 ;; to convert the main s-expression into ROL
 (define (parse-sexpr sexpr)
 (match sexpr
   [(list 'reg-len eq len regE) (cond
                                  [(not (number? len)) (error 'parse-sexpr "len is not a Number" sexpr)]
                                  [(null? regE) (error 'parse-sexpr "regE is null" sexpr)]
                                  [else (parse-sexpr-RegL regE len)])];; remember to make sure specified register length is at least 1
   [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


 (: parse-sexpr-RegL : Sexpr Number -> RegE)
 ;; to convert s-expressions into RegEs
 (define (parse-sexpr-RegL sexpr reg-len)
 (match sexpr
   [(list (and a (or 1 0)) ... ) (if (eq? (len a) reg-len) (Reg(list->bit-list a)) (error 'parse-sexpr "wrong number of bits in ~s" a))]
   [(list 'shl list) (Shl (parse-sexpr-RegL list reg-len))]
   [(list 'and list1 list2) (And (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
   [(list 'or list1 list2) (Or (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
   [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


 (: parse : String -> RegE)
 ;; parses a string containing a RegE expression to a RegE AST
 (define (parse str)
 (parse-sexpr (string->sexpr str)))

;; a function that calculate list length
(: len : (Listof Any) -> Number)
(define (len a)
  (cond
    [(null? a) 0]
    [else (+ 1 (len (rest a)))]))


 ;; tests:
 (test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
 (test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
 (test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg'(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
 (test (parse "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
 (test (parse "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
 (test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")


;;this question was not easy to understand but once i got it it was easy.


#|===========================================================Q.2=============================================================|#

#|
1.
the problem is that you can over ride with set the value in the memory BUT you can not know what value got over ride by who and who go first
in this expresion "{* {+ {set 1} {set 2}} get}" (the first set or the second one).
this problem can be solve by putting curly brackets "{}" in every first <MAE> after the arithmetic expresion like this:
<MAE> ::= <num>
 | { + {<MAE>} <MAE> }
 | { - {<MAE>} <MAE> }
 | { * {<MAE>} <MAE> }
 | { / {<MAE>} <MAE> }
 | { set <MAE> }
 | get

and now it "knows" to calculate the one with the brackets first.


2.
** EA is the BNF from class.

T::= {seq <B>}
    |{seq <AE>}

B::= {set <AE>}
    |{set <AE>} <D>

D::= {set <X>}
     |<D> <D>

X::= {+ <Y> <Y>}
    |{- <Y> <Y>}
    |{* <Y> <Y>}
    |{/ <Y> <Y>}

Y::= <X>
   | <NUM>
   | get

3 different BNF expressions:
<T> => {seq <B>} => {seq {set <AE>}} => {seq {set {* <AE> <AE>}}} => {seq {set {* 204 397}}}
<T> => {seq <B>} => {seq {set <AE>}<D>} => {seq {set {* <AE> <AE>}} {set <X>}} => {seq {set {* 2 0}} {set {* <Y> <Y>}}} => {seq {set {* 2 0}} {set {* get get}}} 
<T> => {seq <AE>} => {seq {- <AE> <AE>}} => {seq {- 2 {* 0 4}}}

this was very hard to get. i had to ask some friends and gets some hints
|#

#|===========================================================Q.3=============================================================|#


(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares a)
  (foldl + 0 (map square a)))


(: square : Number -> Number)
(define (square a)
  (expt a 2))

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(1 2 3 4)) => 30)
(test (sum-of-squares '(1 2 3 4 5)) => 55)
(test (sum-of-squares '(1 2.5 3 4.7 5 6.01 7 8 0.9)) => 213.2701)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(0 0 1 0)) => 1)
(test (sum-of-squares '(1 1 1 1 1)) => 5)
(test (sum-of-squares '(5 5 5 5)) => 100)
(test (sum-of-squares '(-5 -5 -5 -5)) => 100)
(test (sum-of-squares '(-1 -2 -3 -4)) => 30)
(test (sum-of-squares '(1 -2 3 -4)) => 30)
(test (sum-of-squares '(-0)) => 0)
(test (square 0) => 0)
(test (square 1) => 1)
(test (square 01) => 1)
(test (square 4) => 16)
(test (square 22) => 484)
(test (square 100) => 10000)
(test (square -2) => 4)
(test (square -1) => 1)
(test (square -100) => 10000)

;; easy one, was fun to do it
#|===========================================================Q.4=============================================================|#
;; define a binary tree 
(define-type BINTREE
  [Leaf Number]
  [Node BINTREE BINTREE])


(: tree-map : (Number -> Number) BINTREE -> BINTREE)
;; tree map is a function that make arithmetic changes is the tree in every node
(define (tree-map func tree)
  (cases tree
    [(Leaf n) (Leaf (func n))]
    [(Node BT1 BT2) (Node (tree-map func BT1) (tree-map func BT2))]
  )  
)

(: tree-fold : All (A) (A A -> A) (Number -> A) BINTREE -> A)
;; tree fold is like the function foldl but for tree.
;; i used (A A -> A) to make it generic for every king for function to succside working.
;; and (Number -> A) to generate a number into what ever kind i wanna use.
(define (tree-fold func init tree)
  (cases tree
    [(Leaf n) (init n)]
    [(Node BT1 BT2) (func (tree-fold func init BT1) (tree-fold func init BT2))]
  )
)

(: tree-flatten : BINTREE -> (Listof Number))
;; flattens a binary tree to a list of its values in
;; left-to-right order
(define (tree-flatten tree)
 (tree-fold (inst append Number) (inst list Number) tree)
)

(: tree-reverse : BINTREE -> BINTREE)
;; this function reverse the tree. i used a auxiliary function switch-nodes to switch every two sub trees.
;; i use tree-fold and gives "switch-nodes" and "Leaf" as a functions that do this (Number -> A).
(define (tree-reverse tree)
  (tree-fold switch-nodes Leaf tree)
)

(: switch-nodes : BINTREE BINTREE -> BINTREE)
;;auxiliary function that switch sub trees place and Leafs also.
(define (switch-nodes bt1 bt2)
  (Node bt2 bt1)
)

;;Test:
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map add1 (Node (Node (Leaf 7) (Leaf 11)) (Node (Leaf 8) (Leaf 8)))) => (Node (Node (Leaf 8) (Leaf 12)) (Node (Leaf 9) (Leaf 9))))
(test (tree-map sub1 (Node (Leaf 2) (Leaf 3))) => (Node (Leaf 1) (Leaf 2)))

(test (tree-reverse (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Node (Leaf 3) (Leaf 2)) (Leaf 1)))
(test (tree-reverse (Node (Node (Leaf -9)(Leaf 1)) (Node (Leaf 2) (Leaf 3)))) => (Node (Node (Leaf 3) (Leaf 2)) (Node (Leaf 1) (Leaf -9))))
(test (tree-reverse (Node (Leaf 1) (Node (Node (Leaf -90) (Leaf 10)) (Node (Leaf 45) (Leaf -12))))) => (Node (Node (Node (Leaf -12) (Leaf 45)) (Node (Leaf 10) (Leaf -90))) (Leaf 1)))

(test (tree-flatten (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => '(1 2 3))
(test (tree-flatten (Node (Node (Leaf -9)(Leaf 1)) (Node (Leaf 2) (Leaf 3)))) => '(-9 1 2 3))
(test (tree-flatten (Node (Leaf 1) (Node (Node (Leaf -90) (Leaf 10)) (Node (Leaf 45) (Leaf -12))))) => '(1 -90 10 45 -12))

;; not the easiest but i had fun.