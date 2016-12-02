;Name: Muhammad Mustafa
;Student#: 100823576
;COMP 3007: ASSIGNMENT#1
;all the coding for this assignment was done in Scheme R5RS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q1 a)
(display "Q1a) Expressing 2+4*(3*4+6*7)*9 as Scheme procedure: (+(*(*(+(* 3 4)(* 6 7))9)4)2)")
(display " and it calculates to: ")

(+(*(*(+(* 3 4)(* 6 7))9)4)2)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q1 b)
(display "Q1b) Expressing -1+(3+4*1-6)*3/(7*2) as Scheme procedure: (+(/(*(-(+(* 4 1)3)6)3)(* 7 2))-1)")
(display " and it calculates to: ")

(+(/(*(-(+(* 4 1)3)6)3)(* 7 2))-1)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display "TESTING")
(newline)
(display "--------")
(newline)
(newline)

;Q2 a)
;For a function f
;f(n) = n, if n<4 
;f(n) = f(n-1) + 2f(n-2) + 3f(n-3) + 4f(n-4), otherwise
(display "Q2a) Procedure that computes f by means of a recursive process: ")
(newline)

(define (frec n)
  (cond ((< n 4)n)
        (else
         (+(frec (- n 1))
           (*(frec (- n 2))2)
           (*(frec (- n 3))3)
           (*(frec (- n 4))4)))))

(display "testing for lower bounds. input: n = -4; expected output: -4; observed output: ")(frec -4)
(display "testing for lower bounds. input: n = 3; expected output: 3; observed output: ")(frec 3)
(display "testing for base case.    input: n = 4; expected output: 10; observed output: ")(frec 4)
(display "testing for upper bounds. input: n = 5; expected output: 26; observed output: ")(frec 5)
(display "testing for upper bounds. input: n = 20; expected output: 25969271; observed output: ")(frec 20)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q2 b)
(display "Q2b) Procedure that computes f by means of a iterative process: ")
(newline)

(define (fitr n)
  (cond ((< n 4)n)
        (else
         (f-itr 3 2 1 0 n))))
(define (f-itr a b c d count)
  (cond((< count 4)a)
       (else
        (f-itr (+ a (* 2 b) (* 3 c) (* 4 d)) a b c (- count 1)))))

(display "testing for lower bounds. input: n = -4; expected output: -4; observed output: ")(frec -4)
(display "testing for lower bounds. input: n = 3; expected output: 3; observed output: ")(frec 3)
(display "testing for base case.    input: n = 4; expected output: 10; observed output: ")(frec 4)
(display "testing for upper bounds. input: n = 5; expected output: 26; observed output: ")(frec 5)
(display "testing for upper bounds. input: n = 20; expected output: 25969271; observed output: ")(frec 20)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q3)
;Writing a procedure that computes elements of Pascal's triangle by means of a recursive process
;The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it.
;E.g., (pascals 0 0) → 1
;(pascals 2 1) → 2 
;(pascals 4 2) → 6
(display "Q3) Writing a procedure that computes elements of Pascal's triangle by means of a recursive process:")
(newline)

(define (pascals a b)
  (cond ((> b a) "This is an invalid entry: b>a")
        ((= a b)1)
        ((= a b 0)1)
        ((= b 0)1)
        ((= b 1)a)
        ((= b (- a 1))a)
        (else
         (+ (pascals (- a 1) (- b 1)) (pascals (- a 1) b)))))

(display "testing for b>a. input: a = 3, b = 4; expected output: invalid; observed output: ")(pascals 3 4)
(display "testing for a=b. input: a = 3, b = 3; expected output: 1; observed output: ")(pascals 3 3) 
(display "testing for a=b=0. input: a = 0, b = 0; expected output: 1; observed output: ")(pascals 0 0)
(display "testing for b=0. input: a = 3, b = 0; expected output: 1; observed output: ")(pascals 3 0)
(display "testing for b=1. input: a = 3, b = 1; expected output: 3; observed output: ")(pascals 3 1)
(display "testing for b=a-1. input: a = 3, b = 2; expected output: 3; observed output: ")(pascals 3 2)
(display "testing for regualr case. input: a = 4, b = 2; expected output: 6; observed output: ")(pascals 4 2)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q4)
;Rewrting an interative process for a given recursive process to calculate summation of integers between given 2 numbers, using the helper functions provided
;recursive processis is:
;(define (sum term a next b)
;  (if (> a b)
;    0
;    (+ (term a)
;       (sum term (next a) next b))))
;(define (sum-integers a b)
;	(sum identity a inc b))

;helper functions:
(define (inc x) (+ x 1))
(define (identity x) x)

(display "Q4) Summation of integers between 2 integers:")
(newline)

(define (sum a b)
  (cond ((> a b)"invalid range")
        (else
         (sum-int a b 0))))
(define (sum-int a b count)
  (cond ((> a b)count)
        (else
         (sum-int (inc a) (identity b) (+ count a)))))

(display "testing for invalid enteries where b<a. input: a = 5, b = 4; expected output: invalid; observed output: ")(sum 5 4)
(display "testing for a=b=0. input: a = 0, b = 0; expected output: 0; observed output: ")(sum 0 0)
(display "testing for regular entries. input: a = 0, b = 10; expected output: 55; observed output: ")(sum 0 10)
(display "testing for higer values. input: a = 20, b = 40; expected output: 630; observed output: ")(sum 20 40)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q5 a)
;Doing Q4 as a product instead of the summation of integers b/w 2 integers, as recursive process
(display "Q5a) Doing Q4 but as a product of integers b/w 2 integers as a recursive process:")
(newline)

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))
(define (product-integers a b)
	(product identity a inc b))

(display "testing for invalid enteries where b<a. input: a = 5, b = 4; expected output: 1; observed output: ")(product-integers 5 4)
(display "testing for a=b=0. input: a = 0, b = 0; expected output: 1; observed output: ")(product-integers 0 0)
(display "testing for regular entries. input: a = 0, b = 10; expected output: 0; observed output: ")(product-integers 0 10)
(display "testing for regular entries. input: a = 1, b = 10; expected output: 3628800; observed output: ")(product-integers 1 10)
(display "testing for higer values. input: a = 20, b = 30; expected output: 2180547008640000; observed output: ")(product-integers 20 30)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q5 b)
;Calculating the factorial using the product above
(display "Q5b) Defining factorial in terms of product as done in Q5a:")
(newline)

(define (factorial n)
  (if(or(= n 1) (= n 0))
     1
     (product identity 1 inc n)))

(display "tetsing for base case. input: n = 0; expected output: 1; observed output: ")(factorial 0)
(display "tetsing for base case. input: n = 1; expected output: 1; observed output: ")(factorial 1)
(display "tetsing for regular case. input: n = 5; expected output: 120; observed output: ")(factorial 5)
(display "tetsing for higher case. input: n = 10; expected output: 3628800; observed output: ")(factorial 10)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q5 c)
;Rewriting the recursive product in an iterative process
(display "Q5c) Rewriting the recursive product in an iterative process:")
(newline)
(define (product-itr a b)
  (cond ((> a b)"invalid range")
        (else
         (product-int a b 1))))
(define (product-int a b count)
  (cond ((> a b)count)
        (else
         (product-int (inc a) (identity b) (* count a)))))

(display "testing for invalid enteries where b<a. input: a = 5, b = 4; expected output: 1; observed output: ")(product-itr 5 4)
(display "testing for a=b=0. input: a = 0, b = 0; expected output: 1; observed output: ")(product-itr 0 0)
(display "testing for regular entries. input: a = 0, b = 10; expected output: 0; observed output: ")(product-itr 0 10)
(display "testing for regular entries. input: a = 1, b = 10; expected output: 3628800; observed output: ")(product-itr 1 10)
(display "testing for higer values. input: a = 20, b = 30; expected output: 2180547008640000; observed output: ")(product-itr 20 30)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q6)
;(define (p)(p))
;(define (test x y)
;   (if (= x 0)
;      0
;      y))
;	
;(test 0 (p))
;
;a) For the applicative-order the expression in never evaluated and is stuck in an infinite loop.
;  The reason behind this is for an applicative-order each subexpression has to evaluated first. On evaluating it in applicative order, the first subexpression
;  evaluates to be binded to a procedure. The second sub-expression results in a 0; while evaluating the last subexpression, it gets stuck in an infinte loop,
;  since (p) calls upon (p) reccursively.
;b) For the normal-order the substitution is made withing the body of test first, hence, going into the if, when x=0, we just return 0. The expression
;  can be evaluated to be true, thus 0 is returned.
;c) Scheme R5RS uses appllicative-order.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q7)
;Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x,
;then a better approximation is given by the value: (x/y2+2y)/3
;Use this formula to implement a cube-root procedure analogous to the square-root procedure from the lecture notes.
(display "Q7) Making a cube-root function using the lecture notes and function given: (x/y2+2y)/3 for a better approximation:")
(newline)

(define(cube x)
  (* x x x))
(define(cubrt x)
  (cubrt-iteration 1.0 x))
(define (good-enough? guess x)
  (<(abs(-(cube guess) x))0.001))
(define (improve guess x)
  (/(+(/ x (* guess guess)) (* 2 guess))3))
(define (cubrt-iteration guess x)
 (if (good-enough? guess x)
     guess
     (cubrt-iteration (improve guess x) x)))

(display "testing cuberoot for x = 27 is: ")(cubrt 27)
(display "testing cuberoot for x = 64 is: ")(cubrt 64)
(display "testing cuberoot for x = 216 is: ")(cubrt 216)
(display "testing cuberoot for x = 125 is: ")(cubrt 125)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q8)
;(define (a-b a b)
;   ((cond ((> b 0)
;           +)
;          ((= b 0)
;           -)
;          (else *))
;    a b))
;
;CASE I: b>0:
;(define (a-b a b)
;   ((cond ((> b 0)+) ((= b 0)-) (else *) a b)
;   ((cond ((#t)+) ((= b 0)-) (else *) a b)
;   (a+b)
;eg: (a-b 5 16)
;    ((cond ((> 16 5)+) ((= 16 0)-) (else *) a b)
;    ((cond ((#t)+) ((= 16 0)-) (else *) a b)
;    (5+16) ---> 21
;
;CASE II: b=0:
;(define (a-b a b)
;   ((cond ((> b 0)+) ((= b 0)-) (else *) a b)
;   ((cond ((> b 0)+) ((#t)-) (else *) a b)
;   (a-b)
;eg: (a-b 5 0)
;    ((cond ((> 0 0)+) ((= 0 0)-) (else *) a b)
;    ((cond ((> 0 0)+) ((#t)-) (else *) a b)
;    (5-0) ---> 5

;CASE III: b<0:
;(define (a-b a b)
;   ((cond ((> b 0)+) ((= b 0)-) (else *) a b)
;   ((cond ((#f)+) ((#f)-) (else *) a b)
;   (a*b)
;eg: (a-b 4 -4)
;    ((cond ((> -4 0)+) ((= -4 0)-) (else *) a b)
;    ((cond ((#f)+) ((#f)-) (else *) a b)
;    (4*-4) ---> -16

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q9 a)
;Using Newton's approximation for square root finding as shown in the Scheme notes modify the block structured form of the algorithm in order to allow
;a user-defined procedure for good-enough? to be used in the algorithm. The user-defined procedure should be passed into the square-root-finding procedure.
(display "Q9a )Using Newton's approximation for square root finding as shown in the notes we, modify the block structured form of the algorithm in order to allow
    a user-defined procedure for good-enough? to be used in the algorithm. The user-defined procedure should be passed into the square-root-finding procedure: ")

;1st modified goof-enough? fucntion called good-enough?1 with a differernt accuracy of 0.01
(define (good-enough?1 guess x) 
    (<(abs (- (square guess) x)) 0.065))
;2nd modified goof-enough? fucntion called good-enough?1 with a differernt accuracy of 0.1
(define (good-enough?2 guess x) 
    (<(abs (- (square guess) x)) 5))
;3rd modified goof-enough? fucntion called good-enough?1 with a differernt accuracy of 0.0005
(define (good-enough?3 guess x) 
    (<(abs (- (square guess) x)) 15))
;This the default good-enough? function
(define (good-enough? guess x) 
    (<(abs (- (square guess) x)) 0.001))
(define (average x y) 
    (/ (+ x y) 2))
(define (improve guess x) 
    (average guess (/ x guess)))
(define (sqrt-iteration guess x userfcn) 
    (if(userfcn guess x) 
            guess 
            (sqrt-iteration (improve guess x) x userfcn)))
(define (my-sqrt x userfcn) 
    (sqrt-iteration 1.0 x userfcn))
(define (square x)(* x x))

(display "testing default goood-enough? (accuracy is 0.001 for x = 16 is: ")(my-sqrt 16 good-enough?)
(display "testing goood-enough?1 (accuracy is 0.065 for x = 16 is: ")(my-sqrt 16 good-enough?1)
(display "testing goood-enough?2 (accuracy is 5 for x = 16 is: ")(my-sqrt 16 good-enough?2)
(display "testing goood-enough?3 (accuracy is 15 for x = 16 is: ")(my-sqrt 16 good-enough?3)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q9 b)
;Doing Q9a, but instead forcing the termination of the algorithm after a maximum number of iterations.
;EG: (my-sqrt 25 good-enough1 3)→5.406026962727994
(display "Q9b) Doing Q9a, but instead forcing the termination of the algorithm after a maximum number of iterations. ")
(newline)

(define (sqrt-iteration guess x userfcn n counter)
    (if (or (userfcn guess x) (> counter n))
            guess 
            (sqrt-iteration (improve guess x) x userfcn n (+ 1 counter))))
(define (my-sqrt x userfcn n)
     (define counter 1)
    (sqrt-iteration 1.0 x userfcn n counter))
(define (square x) ( * x x))

(display "testing default goood-enough? wtih interation 2 (accuracy is 0.001 for x = 16 is: ")(my-sqrt 16 good-enough? 2)
(display "testing goood-enough?1 wtih interation 5 (accuracy is 0.065 for x = 16 is: ")(my-sqrt 16 good-enough?1 5)
(display "testing goood-enough?2 wtih interation 1 (accuracy is 5 for x = 16 is: ")(my-sqrt 16 good-enough?2 1)
(display "testing goood-enough?3 wtih interation 8 (accuracy is 15 for x = 16 is: ")(my-sqrt 16 good-enough?3 8)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Q9 c)Create a new-if procedure as below:
;(define (new-if predicate consequent alternate)
;   (cond (predicate consequent)
;         (else alternate)))
;Replace the use of if in sqrt-iter with new-if. Does the new version work? Explain why or why not.
;
;Answer) It doesn't seem to work. It gets stuck in an infinite loop.
;       Reason: Since Scheme uses applicative-order to evaluate prcedures. Hence here, in the new-if the operands are evaluated first...but the second operand
;               is a recursice call to sqrt-iteration, thu it gets stuck in an infinite loop.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  THE END  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;