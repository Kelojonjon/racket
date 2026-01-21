#lang racket/base


;; Lets follow along with the sicp see if we can learn something

; Sum of 2 squares
; Mostly the idea of showing this fuunction seems to be to showcase how
; inefficient substitution can be if we for example input our operands as 
; expressions that havent been finished
; Using the applicative evaluation we only carry the evaluated values
; around instead of forming a big list of expressions, thus often making it a lot
; more cleaner than substiting each and every X in the expression by a unevaluated expression :D
(define (sum_of_squares x y)
  (+ (* x x) (* y y)))

;; Definition of abs using cond
(define (abs x)
  (cond
    [(< x 0) (- x)]
    [else x]))

; Seeing the above definition of abs we can see that
; operator - and + are unary operators, you can negate stuff!
(define (unaries x)
  (let (
        [plus (+ x)]
        [minus (- x)]
        )(list plus minus)))

;; This time we are defining abs using if 
;; we can see it branching into 3 different branches
;; Difference with cond and if here along with the branching is that cond
;; allows one to freely "chain" expressions after the predicate, where as the if
;; wants each expression to be a single expression! 
(define (if_abs x)
  (if (< x 0)
    (- x)
    x))

;; Exercise 1.3
;; Define a procedure that takes 3 nunmbers as arguments and returns the sum of the 2 larger numbers
;; There are more cases we could encounter like 2 numbers being equal, but we are gonna leave this here
(define (sum_of_squares_for_3 a b c)
  (cond
    [(and (< a b) (< a c)) (sum_of_squares b c)]
    [(and (< b c) (< b a)) (sum_of_squares c a)]
    [(and (< c a) (< c b)) (sum_of_squares a b)]
    [else (displayln "Arrghhh confusion!")]))


;; Here we are completely ignoring the case where there could be 2 variables that are the same size
;; But it really doesnt matter here does it? does it?
(define (sum_of_squares_for_3_2 a b c)
  (let (
        [sorted  (cdr (sort (list a b c) <))]
        )(sum_of_squares (car sorted) (car (cdr sorted)) )))

;; Exercise 1.4
;; Here we can see how lisp allows one to return operators as "values" until they are executed
;; This allows us to define 2 operators for a and b based on a condition, only using 1 set of a and b
;; As the operator is not being evaluated until after the if block is being evaluated, it is not handled as a
;; operator until the (+ a b) // (- a b) case happens
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))


;; Exercise 1.5
;; The following testa ae supposed to reveal wether we are using applicative or normal-order evaluation
;; Seems like the (p) is basicly a recursive function that calls itself with no exit condition
;; We can hear this in action, by the computer suddenly ramping up, and TOP showing our cpu usage hitting 100%
(define (p) (p))
  
;; (test 0 (p))
;; Here we can see the infinite loop in action, as our test should return 0 when we declare x to be 0
;; But because racket is applicative, it tries to evaluate the arguments before moving on to the if test
;; This causes us to be thrown in a infinite loop before ever even getting to the if test
;; If we were using normal-order, we would first substiture the stuff to its, place, thus giving us the window
;; to run the if test, and then run the "success" branch 0 instead of starting the infinite lop again
(define (test x y)
  (if (= x 0) 0 y))


;; 1.1.7 Square roots by newtons method
;; I have already done square roots by binary search. Newtons method should cut the steps
;; pretty agressively resulting in faster completion-times.
;; We can try to see if we bring my "search range" and "accuracy" parameters here,
;; This would allow us to estimate a good "seed" for the square root, making the algorithm even faster.
;; We will then feed the result to the equation again, to make it more accurate, until we are satisfied with our result

;; I will write a helper that will handle our tolerance
(define (center_interval center diameter)
  (let* (
        [radius (/ diameter 2)]
        [point_a (- center radius)]
        [point_b (+ center radius)] 
        )(list point_a point_b)))

;; This is the actual algorithm doing the calculation, as we dont want to calculate the
;; tolerances on every run, we will instead pass create a wrapper, and then pass the tolerances in
;; along with inside the function operands
(define (math_newton_sqrt target x tol_low tol_up)
  (let* (
        [new_x (/ (+ x (/ target x)) 2)]
        [square (* x x)]
        )
    (cond
      [(and (>= square tol_low) (<= square tol_up)) x]
      [else (math_newton_sqrt target new_x tol_low tol_up)])))

;; The wrapper for the math_newton_sqrt
(define (newton_sqrt target x tol)
  (let* (
        [tolerance (center_interval target tol)]
        [tol_low (car tolerance)]
        [tol_up (car (cdr tolerance))]
        )(math_newton_sqrt target x tol_low tol_up)))


;; Exercise 1.6
;; What happens when you try to run sqrt iter, using the new-if functiion
(define (improve guess x)
  (average guess (/ x guess)))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
    (else else-clause)))
;; Because racket uses applicative order, it means the moment we run sqrt-iter it
;; tries to evaluate from the "smallest bubble" upwards. This means our recursive
;; sqrt iter call gets evaluated every time we we run sqrt-iter, making it recurse
;; uncontrollably
;; This shows the specialty of cond and if, which are special cases.
;; With cond and if, we evaluate the branches using normal-order, to prevent us prematurely
;; evaluating the "then-clause" and "else-clause" branches.
;; Without these special cases recursion wouldnt be possible!
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
  guess
  (sqrt-iter (improve guess x) x)))


;; Exercise 1.7
;; This good-enough? machine has a limited usecase for big or small numbers, because of its fixed comparison
;; Can we make it better?
;; I already handled this using "tolerance" in my own version of the square root, but lets improve the 
;; function given to us in the book
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; We can use a percentage to create a new better good-enough?
(define (percent perc num)
  (* perc num))

;; Even if this works, i would still prefer the "tolerance" method, that allows us to
;; choose our accuracy from the "number line" instead of a percentage, because we can 
;; contain the answer inside "stable intervall"
(define (better_good_enough guess x [perc 0.0001])
  (< (abs (- (square guess) x)) (percent perc x)))

;; Sicp suggests using the "percentage of change" as a way to handle this,
;; I would say its probably the best of these for general use because it scales automaticly,
;; unlike my approaches, that need some care when choosing the parameters
;; Since i dont have the original sqrt method, we can just write the logic for
;; the percentage of change check which is pretty simple
(define (percentage_change new_value old_value)
  (abs (- 100 (* 100 (/ new_value old_value)))))

;; Exercise 1.8
;; Take a cube root of a number using (x/y² + 2y) / 3
;; We need to use floatiing points when we input our stuff, to ever get a answer, because the fractions 
;; the algorithm generates, get exponentially bigger, thus never finishing the calculation
;; Here we can see that binary search is actually much more accurate, because we can slowly get to our tolerances
;; While newtons method is superior for quickly getting a "good enough" result, but sruggles with exact answers
;; (that go into more precision than neighbours cats gravitational pull across your wall)
(define (newton_crt target x [perc 0.001])
  (let* (
         [new_x (/ (+ (* 2 x) (/ target (expt x 2))) 3)]
         [perc_change (percentage_change new_x x)]
         )
    (cond
      [(< perc_change perc) new_x]
      [else (newton_crt target new_x perc)])))

;; Racket allows us to use "blocks" and "lexical scoping" to structure out programs
;; In other words we can define procedures inside other procedures, thus freeing namespace or the main file
;; Lexical scoping means just that as we define procedures inside procedures, they share the same scope (I assume the "higher"
;; function doesnt share the "child" functions scopes though)
(define (mother_function x y)
  (define (lexical_add)
    (+ x y))
  (define (non_lexical_sub x y)
    (- x y))
  (list (lexical_add) (non_lexical_sub x y)))


