  ;; The Flang interpreter

  #lang pl

  #|
  The grammar:
    <FLANG> ::= <num>
              | { + <FLANG> <FLANG> }
              | { - <FLANG> <FLANG> }
              | { * <FLANG> <FLANG> }
              | { / <FLANG> <FLANG> }
              | { with { <id> <FLANG> } <FLANG> }
              | <id>
              | { fun { <id> } <FLANG> }
              | { call <FLANG> <FLANG> }
              | <True> ;; add rule for True ;; Rule 10
              | <False> ;; Rule 11
              | { < <FLANG> <FLANG>} ;; add rule for = ;; Rule 12
              | { > <FLANG> <FLANG>} ;; Rule 13
              | { = <FLANG> <FLANG>} ;; Rule 14
              | { not <FLANG> } ;; Rule 15
              | { if <FLANG> { then-do <FLANG> } { else-do <FLANG> } } ;; add rule 16 for (the above) if

  Evaluation rules:

   subst:
     N[v/x] = N
     {+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]}
     {- E1 E2}[v/x] = {- E1[v/x] E2[v/x]}
     {* E1 E2}[v/x] = {* E1[v/x] E2[v/x]}
     {/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]}
     y[v/x] = y
     x[v/x] = v
     {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
     {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
     {call E1 E2}[v/x] = {call E1[v/x] E2[v/x]}
     {fun {y} E}[v/x] = {fun {y} E[v/x]} ; if y =/= x
     {fun {x} E}[v/x] = {fun {x} E}
     B[v/x] = B ;; B is Boolean
     {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]}
     {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]}
     {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]}
     { not E}[v/x] = {not E[v/x]}
     {if Econd {then-do Edo} {else-do Eelse}}[v/x] = {if Econd[v/x] {then-do Edo[v/x]} {else-do Eelse[v/x]}}


   eval: Evaluation rules:
     eval(N) = N ;; N is an expression for a numeric value
     eval({+ E1 E2}) = eval(E1) + eval(E2) \ if both E1 and E2
     eval({- E1 E2}) = eval(E1) - eval(E2) \ evaluate to numbers
     eval({* E1 E2}) = eval(E1) * eval(E2) / otherwise error!
     eval({/ E1 E2}) = eval(E1) / eval(E2) /
     eval(id) = error!
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
     eval(FUN) = FUN ; assuming FUN is a function expression
     eval({call E1 E2}) = eval(Ef[eval(E2)/x]) if eval(E1)={fun {x} Ef} = error! otherwise

     eval(B) = B ;; B is an expression for a Boolean value
     eval({= E1 E2}) = eval(E1) = eval(E2) \ if both E1 and E2
     eval({> E1 E2}) = eval(E1) > eval(E2) \ evaluate to numbers
     eval({< E1 E2}) = eval(E1) < eval(E2) / otherwise error!
     eval({not E}) = not(eval(E)) /E may be anything
     eval({if Econd {then-do Edo} {else-do Eelse}}) = eval(Edo) if eval(Econd) =/= false,
     eval(Eelse), otherwise.
  |#
#| in this function i defined the new operators and a boolean|#
  (define-type FLANG
    [Num  Number]
    [Add  FLANG FLANG]
    [Sub  FLANG FLANG]
    [Mul  FLANG FLANG]
    [Div  FLANG FLANG]
    [Id   Symbol]
    [With Symbol FLANG FLANG]
    [Fun  Symbol FLANG]
    [Call FLANG FLANG]
    [Bool Boolean]
    [Bigger FLANG FLANG]
    [Smaller FLANG FLANG]
    [Equal FLANG FLANG]
    [Not FLANG]
    [If FLANG FLANG FLANG])


#| in this function i parse the new operators. you can see that if have syntex a bit diffrent so i menaged it diffrently|#
  (: parse-sexpr : Sexpr -> FLANG)
  ;; to convert s-expressions into FLANGs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(number: n)    (Num n)]
      ['True (Bool true)]
      ['False (Bool false)]
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body) (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name)) body) (Fun name (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
      [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '< lhs rhs)  (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '> lhs rhs)  (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'not exp) (Not (parse-sexpr exp))]
      [(cons 'if more)
       (match sexpr
         [(list 'if condition (list 'then-do do) (list 'else-do else)) (If (parse-sexpr condition) (parse-sexpr do) (parse-sexpr else))]
         [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

  (: parse : String -> FLANG)
  ;; parses a string containing a FLANG expression to a FLANG AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))


#|this function is pretty much the same.. i added the new operator and make them like the other ones|#
  (: subst : FLANG Symbol FLANG -> FLANG)
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (subst expr from to)
    (cases expr
      [(Num n) expr]
      [(Add l r) (Add (subst l from to) (subst r from to))]
      [(Sub l r) (Sub (subst l from to) (subst r from to))]
      [(Mul l r) (Mul (subst l from to) (subst r from to))]
      [(Div l r) (Div (subst l from to) (subst r from to))]
      [(Id name) (if (eq? name from) to expr)]
      [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
      [(Call l r) (Call (subst l from to) (subst r from to))]
      [(Fun bound-id bound-body)
       (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]
      [(Bool b) expr]
      [(Equal l r) (Equal (subst l from to) (subst r from to))]
      [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
      [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
      [(Not exp) (Not (subst exp from to))]
      [(If  l m r) (If (subst l from to) (subst m from to) (subst r from to))]))


   ;; The following function is used in multiple places below,
   ;; hence, it is now a top-level definition
   (: Num->number : FLANG -> Number)
   ;; gets a FLANG -- presumably a Num variant -- and returns the
   ;; unwrapped number
   (define (Num->number e)
    (cases e
     [(Num n) n]
     [else (error 'Num->number "expected a number, got: ~s" e)]))


   (: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
   ;; gets a Racket numeric binary operator, and uses it within a FLANG
   ;; `Num' wrapper
   (define (arith-op op expr1 expr2)
    (Num (op (Num->number expr1) (Num->number expr2))))


#|this function is very simular to the arith-op so i made the same with Bool insted of Num|#
   (: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
   ;; gets a Racket Boolean binary operator (on numbers), and applies it
   ;; to two `Num' wrapped FLANGs
   (define (logic-op op expr1 expr2)
   (Bool (op (Num->number expr1) (Num->number expr2))))


#|this function was a bit tricky, u had to read the commends like 6 time before i got what i need to do. if it false then return false else true|#
   (: flang->bool : FLANG -> Boolean)
   ;; gets a Flang E (of any kind) and returns a its appropiate
   ;; Boolean value -- which is true if and only if E does not
   ;; represent false
   ;; Remark: the `flang->bool` function will also be top-level
   ;; since it's used in more than one place.
   (define (flang->bool e)
    (cases e
       [(Bool b) b]
       [else true]))


#|here i added the operators. =,>,<, and Bool was easy, 'not' was a bit tricky and if took me some time to anderstand that i need to use flang->bool|#
  (: eval : FLANG -> FLANG)
  ;; evaluates FLANG expressions by reducing them to *expressions*
  (define (eval expr)
    (cases expr
      [(Num n) expr]
      [(Add l r) (arith-op + (eval l) (eval r))]
      [(Sub l r) (arith-op - (eval l) (eval r))]
      [(Mul l r) (arith-op * (eval l) (eval r))]
      [(Div l r) (arith-op / (eval l) (eval r))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (eval named-expr)))]
      [(Id name) (error 'eval "free identifier: ~s" name)]
      [(Fun bound-id bound-body) expr]
      [(Call fun-expr arg-expr)
       (let ([fval (eval fun-expr)])
         (cases fval
           [(Fun bound-id bound-body) (eval (subst bound-body bound-id (eval arg-expr)))]
           [else (error 'eval "`call' expects a function, got: ~s"
                              fval)]))]
      [(Bool b) expr]
      [(Equal l r) (logic-op = (eval l)(eval r))]
      [(Bigger l r) (logic-op > (eval l)(eval r))]
      [(Smaller l r) (logic-op < (eval l)(eval r))]
      [(If l m r)
        (let ([fval (flang->bool(eval l))])
          (if (equal? fval #t) (eval m) (eval r)))]
      [(Not exp) (Bool (not exp))]))



#|hee i added Bool and got rid of the error cause we can only get Num Bool and Fun back.|#
  (: run : String -> (U Number Boolean FLANG))
    ;; evaluate a FLANG program contained in a string
  (define (run str)
    (let ([result (eval (parse str))])
      (cases result
        [(Num n) n]
        [(Bool b) b]
        [else result])))


#|That it! the assignment was ok. not so easy or hard. here some tests i did to cover of the function. have a nice day! :)|#
  ;; tests
  (test (run "{call {fun {x} {+ x 1}} 4}") => 5)
  (test (run "{with {add3 {fun {x} {+ x 3}}} {call add3 1}}") => 4)
  (test (run "{with {add3 {fun {x} {+ x 3}}} {with {add1 {fun {x} {+ x 1}}} {with {x 3} {call add1 {call add3 x}}}}}") => 7)
  (test (run "True") => true)
  (test (run "{not True}") => false)
  (test (run "{> 3 44}") => false)
  (test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
  (test (run "{with {x 8} {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
  (test (run "{with {x 0} {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
  (test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
  (test (run "{with {c True} {if c {then-do {> 2 1}} {else-do 2}}}") => true)
  (test (run "{with {foo {fun {x} {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}") => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
  (test (run "{with {x 0} {if {> x 0} {/ 2 x} x}}") =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
  (test (run "true") =error> "eval: free identifier: true")
  (test (run "{< false 5}") =error> "eval: free identifier: false")
  (test (run "{< False 5}") =error> "Num->number: expected a number, got: #(struct:Bool #f)")
  (test (run "{with {y True} {if {not y} {then-do False} {else-do {* 6 6}}}}") => 36)
  (test (run "{with {x 8} {if {= x 0} {then-do {/ 2 x}} {else-do x}}}") => 8)
  (test (run "{call {if {= 5 5} {+ 3 1} 4}}") =error> "parse-sexpr: bad syntax in (call (if (= 5 5) (+ 3 1) 4)")
  (test (run "{with {c True} {if c {then-do {< 2 5}} {else-do 2}}}") => true)
  (test (run "{with {c True} {if c {then-do {- 2 5}} {else-do 2}}}") => -3)
  (test (run "{with {if c {then-do {- 2 5}} {else-do 2}}}") =error> "parse-sexpr: bad `with' syntax in (with (if c (then-do (- 2 5)) (else-do 2)))")
  (test (run "{with {foo {fun {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}") =error> "parse-sexpr: bad `fun' syntax in (fun (if (< x 2) (then-do x) (else-do (/ x 2))))")
  (test (run "{call 5 4}") =error> "eval: `call' expects a function, got: #(struct:Num 5)")
  (test (run "{with {add3 {fun {x} {+ x 3}}} {with {x 1} {call add3 x}}}") => 4)
   