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

;; Here we can see how lisp allows one to return operators as "values" until they are executed
;; This allows us to define 2 operators for a and b based on a condition, only using 1 set of a and b
;; As the operator is not being evaluated until after the if block is being evaluated, it is not handled as a
;; operator until the (+ a b) // (- a b) case happens
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))


