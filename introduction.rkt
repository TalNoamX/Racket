#lang pl
#|@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Q.1 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@|#

(: plSuffixContained : (Listof String) -> (U String #f))
;; This function is finds the first member in a list which ends with the letters 'pl'.
;; it call the function helpPL and give it every iteration the the next member from the first to the last.
;;if it return #t plSuffixContained return the member else it goes to another iteration. if non contain the the suffix pl return #f.
(define (plSuffixContained list)
  (cond
    [(null? list) #f]
    [(helpPL (first list)) (first list)]
    [else (plSuffixContained (rest list))]))
    
(: helpPL : String -> Boolean)
;; this function check if a string contain the suffix pl and return #t or #f appropriate.
(define (helpPL word)
  (if(and (equal? (string-ref word (- (string-length word) 1)) #\l) (equal? (string-ref word (- (string-length word) 2)) #\p)) #t #f))

;; tests:
(test (helpPL "dsdcsdpl") => #t)
(test (helpPL "dsdcsdpo") => #f)
(test (helpPL "dsdcsdl") => #f)
(test (plSuffixContained '("sdvcercdscsd" "kcmkdlcmspo" "hscsjckpl")) => "hscsjckpl")
(test (plSuffixContained '()) => #f)
(test (plSuffixContained '("sdvcercdscspld" "kcmkdlcmslp" "hscsjckpl" "plplplplplpl")) => "hscsjckpl")
(test (plSuffixContained '("p l" " pl" "pl  ")) => " pl")


#|@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Q.2 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@|#


#| 2.1 |#

(: write-poly : (Listof Number) -> String)
;; This function gets a list of numbers and return a String of polynom in a reversed order of coefficients.
;; use two helping functions: make-poly and plus-minus-sign.
;; using the tecnic tail-recursion.
(define (write-poly list)
  (make-poly list (- (length list) 1) "")
  )



(: make-poly : (Listof Number) Number String -> String)
;; this function build the String for the output.
;; its work with the tecnic tail-recursion which is basically a for loop.
;; it use the plus–minus-sign function to determine if there is a minus or a plus sigh to the memnber of the polynom.
(define (make-poly list counter poly)
  (cond
    [(null? list) poly]
    [else (make-poly (rest list) (- counter 1) (plus–minus-sign (first list) poly counter))]
    )
  )


(: plus–minus-sign : Number String Number -> String)
;;this determine if there is a minus or a plus sigh to the memnber of the polynom with conditions and cover all cases.
(define (plus–minus-sign member poly counter)
  (cond
    [(= member 0) poly]
    [(and (string=? poly "") (= counter 0)) (number->string member)]
    [(string=? poly "") (string-append (number->string member) (string-append "x^" (number->string counter)))]
    [(and (= counter 0) (< member 0)) (string-append poly (number->string member))]
    [(= counter 0) (string-append poly (string-append "+" (number->string member)))]
    [(< member 0) (string-append poly (string-append (number->string member) (string-append "x^" (number->string counter))))]
    [else (string-append poly (string-append (string-append "+" (number->string member)) (string-append "x^" (number->string counter))))]
    )
  )

;;Tests:
(test (write-poly '(4 4 5)) => "4x^2+4x^1+5")
(test (write-poly '()) => "")
(test (write-poly '(-4 -4 -4)) => "-4x^2-4x^1-4")
(test (write-poly '(4 4 5 1 2 3)) => "4x^5+4x^4+5x^3+1x^2+2x^1+3")
(test (write-poly '(0 0 0 0 0 1)) => "1")
(test (write-poly '(0 0 0 0 0 -1)) => "-1")
(test (write-poly '(0 4 0 6 0 0 1)) => "4x^5+6x^3+1")
(test (write-poly '(0)) => "")
(test (write-poly '(0 0 0)) => "")


#| 2.2 |#

(: compute-poly : Number (Listof Number) -> Number)
;; this function is getting a number x and a list of numbers and like the function from 2.1 it make a polynom out of the list
;; but now it compute the value of the polynom with the x.
;; use two auxiliary functions: 1)sum-poly that sum the value of each monom value with x 2)compute that compute the monom value.
(define (compute-poly x list)
  (sum-poly x list 0)
  )


(: sum-poly : Number (Listof Number) Number -> Number)
;; this function sum the value all monoms with tail-recursion. use the compute function for each monom and then sum it
(define (sum-poly x list sum)
  (cond
    [(null? list) sum]
    [else (sum-poly x (rest list) (+ sum (compute (first list) x (- (length list) 1))))]
    )
  )


(: compute : Number Number Integer -> Number)
;;auxiliary function compute the value of monom with value x for X and exp for exponent
(define (compute mem x exp)
  (* mem (expt x exp))
  )

;;Tests:
(test (compute-poly 2 '()) => 0)
(test (compute-poly 0 '(3 2 1)) => 1)
(test (compute-poly 0 '(3 2 0)) => 0)
(test (compute-poly -2 '(3 2 1)) => 9)
(test (compute-poly 2 '(-3 -2 -1)) => -17)
(test (compute-poly 0.5 '(3 2 1)) => 2.75)
(test (compute-poly 1 '(3 2 1)) => 6)
(test (compute-poly 1 '(3 2 1)) => 6)
(test (compute-poly 3 '(3 7 8 2 1)) => 511)
(test (compute-poly 3 '(-3 -7 -8 -2 -1)) => -511)

(test (compute 2 3 2) => 18)
(test (compute 2 -2 2) => 8)
(test (compute -2 5 2) => -50)
(test (compute 0 2 2) => 0)
(test (compute 1 7 0) => 1)
(test (compute 2 4 -2) => 1/8)
(test (compute 2 -2 -2) => 1/2)
(test (compute 2 -2 -3) => -1/4)


#|@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Q.3 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@|#

;; here i define a data structure of a stuck with kay value. work like an onion layers
(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack]
  )

(: search-stack  : Symbol KeyStack -> (U String #f))
;; This function is searching a value with a key in the stuck. it gives the first value with the key it got in the input.
;; use cases and every time the key doesn't much it "peel" another layer of the stuck.
(define (search-stack key KS)
  (cases KS
    [(EmptyKS) #f]
    [(Push k v ks) (if(equal? k key) v (search-stack key ks))]
     )
  )

(: pop-stack : KeyStack -> (U KeyStack #f))
;; This function is the pop action of the stuck. it gets a stuck type and return the same stuck with out the first ket-value member.
;;use cases to return #f if its an empty stuck or the same stuck without the first member.
(define (pop-stack KS)
  (cases KS
    [(EmptyKS) #f]
    [(Push k v ks) ks]
    )
  )


;;Tests:
(test (EmptyKS) => (EmptyKS)) 
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test  (Push 'c "C" (Push 'b "B" (Push 'a "A" (Push 'c "CC" (Push 'd "D" (Push 'd "DD" (EmptyKS))))))) =>  (Push 'c "C" (Push 'b "B" (Push 'a "A" (Push 'c "CC" (Push 'd "D" (Push 'd "DD" (EmptyKS))))))))

(test (search-stack 'a (EmptyKS)) => #f)
(test (search-stack 'b (Push 'a "A" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "B")
(test (search-stack 'c (Push 'a "A" (Push 'b "B" (Push 'c "C" (EmptyKS))))) => "C")
(test (search-stack 'd (Push 'c "C" (Push 'b "B" (Push 'a "A" (Push 'c "CC" (Push 'd "D" (Push 'd "DD" (EmptyKS)))))))) => "D")
(test (search-stack 'e (Push 'e "EEEE" (Push 'e "EE" (Push 'e "EEE" (EmptyKS))))) => "EEEE")
(test (search-stack 'f (Push 'c "C" (Push 'b "B" (Push 'a "A" (Push 'd "D" (Push 'e "E" (Push 'x "X" (EmptyKS)))))))) => #f)

(test (pop-stack (Push 'c "C" (Push 'b "B" (Push 'a "A" (Push 'c "CC" (Push 'd "D" (Push 'd "DD" (EmptyKS)))))))) => (Push 'b "B" (Push 'a "A" (Push 'c "CC" (Push 'd "D" (Push 'd "DD" (EmptyKS)))))))
(test (pop-stack (Push 'b "B" (Push 'a "A" (Push 'c "CC" (Push 'd "D" (Push 'd "DD" (EmptyKS))))))) => (Push 'a "A" (Push 'c "CC" (Push 'd "D" (Push 'd "DD" (EmptyKS))))))
(test (pop-stack (Push 'a "A" (Push 'c "CC" (Push 'd "D" (Push 'd "DD" (EmptyKS)))))) => (Push 'c "CC" (Push 'd "D" (Push 'd "DD" (EmptyKS)))))
(test (pop-stack (Push 'c "CC" (Push 'd "D" (Push 'd "DD" (EmptyKS))))) => (Push 'd "D" (Push 'd "DD" (EmptyKS))))
(test (pop-stack (Push 'd "D" (Push 'd "DD" (EmptyKS)))) => (Push 'd "DD" (EmptyKS)))
(test (pop-stack (Push 'd "DD" (EmptyKS))) => (EmptyKS))
(test (pop-stack (EmptyKS)) => #f)

#|@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Q.4 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@|#


(: is-odd? : Natural -> Boolean)
#|
This function is checking if a natural number is odd.
first it check if the number x is zero, if so return false, else it send (x-1) to the "function is-even?"
it run like this: the input is x, then x goes back and forth between "is-odd?" and "is-even?" functions, if it end up in "is-even?" the number is odd and return "true" if it ends in is-odd? the number is even and return "false", how so?
if number is odd and the run start in "is-odd?", then it should come to be odd when it is on "is-odd?" function and even when it is on "is-even?" function. so it goes till the number turns 0.
if it is zero when x is in  "is-even?" function then x is odd and return "true" else return "false".
at first it was very difficult to understand this function, i just didn't get it, buy then i did a simulation of a run and then it hit me.. those two functions work together perfectly, it was hard to describe it thow.
|#

(define (is-odd? x)
 (if (zero? x)
 false
 (is-even? (- x 1))))

(: is-even? : Natural -> Boolean)
#|
This function is checking if a natural number is even.
first it check if the number x is zero, if so return true, else it send (x-1) to the function "is-eodd?"
it run like this: the input is x, then x send back and forth between "is-odd?" and "is-even?" functions, if it end up in "is-even?" the number is even and return "true" if it ends in "is-odd?" the number is odd and return "false", how so?
if number is even and the run start in "is-even?", then it should come to be even when it is on "is-even?" function and even odd it is on "is-odd?" function. so it goes till the number turns 0.
if it is zero when x is in  "is-even?" function then x is even and return "true" else return "false".
at first it was very difficult to understand this function, i just didn't get it, buy then i did a simulation of a run and then it hit me.. those two functions work together perfectly, it was hard to describe it thow.
|#
(define (is-even? x)
 (if (zero? x)
 true
 (is-odd? (- x 1))))

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; See explanation about the All syntax at the end of the file...
#|
This function purpose is to בheck with all the members in a list that they are legally valid by a condition,
and return true in case all the members meet the condition and false if not using recursion.
the input is a function "pred" that return boolean statment and a list of a variable that the function "pred" works on.
the output is a boolean statment that indicate if all the list members are meet the same condition. can be true or false.
this function was alittle hard to understand because it use a function of some kind, but once i understand that every thing else was pretty understood.
|#
(define (every? pred lst)
 (or (null? lst)
 (and (pred (first lst))
 (every? pred (rest lst)))))
;; An example for the usefulness of this polymorphic function

(: all-even? : (Listof Natural) -> Boolean)
#|
This function is loke the "is-even?" function but! it checks a list of numbers insted of one number.
the input of the function is a list of numbers.
the function call "every?" function and give in the "is-even?" function and the list as input.
we know that "every?" checks if all list's members are fit the same condition, so it checks if all member is even.
the output of the function is the output of the "every?" function, true if all the list's members are even and false if not (it take only one member to get false for all the list).
this function was easier to to figure out because i already know the "every?" and "is-even?" functions.
|#
(define (all-even? lst)
 (every? is-even? lst))

;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))


(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
#|
This function is very similar to the "every?" function, is do the same thing but for two diffrent lists and two diffrent functions that works on the lists,
it take two lists and two functions and check if all the members in the first list is fitting the first condition and the same thing for the other list.
It works in a recursive way, every iteration it send a member of each list to the its function and the rest of the list to the next iteration.
the input is two functions that return boolean statment and two list of members and the functions get as input.
the output is a boolean statment is the member of the list fit the condition. this function check every member of both lists in recursion and if one of the members is both
lists is false all the run turns false.
this function was easier to to figure out because i already know the "every?" function.
|#
(define (every2? pred1 pred2 lst1 lst2)
 (or (null? lst1) ;; both lists assumed to be of same length
 (and (pred1 (first lst1))
 (pred2 (first lst2))
 (every2? pred1 pred2 (rest lst1) (rest lst2)))))
