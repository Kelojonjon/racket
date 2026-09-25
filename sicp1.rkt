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
;; function doesnt share the "child" functions scopes though) <- which is true! well done myself from the past!
(define (mother_function x y)
  (define (lexical_add)
    (+ x y))
  (define (non_lexical_sub x y)
    (- x y))
  (list (lexical_add) (non_lexical_sub x y)))



; 1.2 Procedures and the processes they generate
;
; I assume this chapter is pretty much just decoding what it looks like
; when some hands on procedures execute their call stack
; of course which is just basicly ast tree in its simplest form, until we move to more complex patterns like streams
; Which i havent yet actually done in lisp, lazy streams-n stuff


; Talking about recursive and iterative procedures and processes
; Procedure is our code process is what it summons
; The book uses calculating factorials as a example, we dont have much more imagination so so do we

; Until we hit n = 1, our applicative order keeps on calling stacks with (recursive-procedure (- n 1))
; when we hit 1 we dont need to do any more function calls, and instead the last call collapses into 1
; At this point we have basicly a fully substituted "formula" of the problem, which will cause the process
; to bounce back and reduce its way into the final answer
(define (recursive-procedure n)
  (if (= n 1)
      1
      (* n (recursive-procedure (- n 1)))))

; Iterative approach or how i would call it myself tail-recursive does the operation on the way to the last function-call
; What this means is that we carry all our needed parameters with us manipulate them, and then call the same function
; with the manipulated parameters until we hit a "base-case", honestly i dont know why we should use the traditional
; recursion at all, this iterative approach is much more memory efficient. <- (some mathemathical structures for example
; naturally use traditional recursion making the code much simpler)
(define (iterative-procedure n)
  ;unlike the book we will use let loops instead of definitions to "hide" the smaller
  ;specialized functions from the global scope
  (let loop (
             [prod 1]
             [num 1]
             )
    (if (= num n)
        (* prod num)
        (loop (* prod num) (+ 1 num)))))


; Exercise 1.9
; Two procedures whom both define a method for adding 2 psotitive integers.
; We use procedures inc which increments its argument by 1 and dec which decrements its argument by 1
; Trace the execution of both of the processes and find out is one is iterative or recursive

; Lets first define our inc and dec to make it work
(define (inc n)
  (+ n 1))

(define (dec n)
  (- n 1))

; Procedure A
; We have a traditional recursive function
; We call plus and decrement a until we hit 0, then we return b and start incrementing it until we clear the stack
(define (recursive-plus a b)
  (if (= a 0)
      b
      (inc (recursive-plus (dec a) b))))

; Procedure B
; We have a iterative function here
; We simply modulate the arguments in the function call itself, until we hit a = 0
; then we just return the already modulated value for b
(define (iterative-plus a b)
  (if (= a 0)
      b
      (iterative-plus (dec a) (inc b))))


; Exercise 1.10
; Trace the process of arckermans function
; These are just best done on blank a4, as you can see you can basicly just turn every problem into a nice little tree
; For example our (A 1 10) returns (** 2 10) -> 1024
; Whats the mathemathical rule though?
; For (A 2 4) we get (** 2 16) for (A 1 10) we get (** 2 10)
(define (A x y)
  (cond
    [(= y 0) 0]
    [(= x 0) (* 2 y)]
    [(= y 1) 2]
    [else (A (- x 1)
             (A x (- y 1)))]))


; 1.2.2 Tree recursion
; The book shows us a implementation of fibbionacci sequence
; First to understand the problem we will make our own implementation that will be tail recursive
; Since we rely on knowing the last 2 numbers generated and we havent found a exiplit formula for fibbionacci,
; we start from a premade start, its not the mathemathical definition of the problem but it works wonders
(define (tail-fibbionacci max-gen)
  (let loop (
             [gen 2]
             [result '(1 0)]
             )
    (cond
      [(or (< gen 2) (= gen max-gen)) (reverse result)]
      [else (let (
                  [next-num (+ (car result) (cadr result))]
                  )
              (loop (+ 1 gen) (cons next-num result)))]
      )))

; We could also use a rolling window or scalar version of the same fibbioncci, but in this chapter, we are talking about
; tree recursion, so lets stick to the point.
; This way of recursive computing should create a much more complex "tree-shape" when used to calculate fibbionacci sequences
(define (fib n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else
      (+ (fib (- n 1))
         (fib (- n 2)))]))
; So what exactly is happening here?
; For ever (fib n) we split into 2 child nodes, this creates the tree shape automaticly
; Each leave hits the "base case" at some point collapsing the recursive funtion call into a value
; For each horizontal layer in the tree we created (fib n - 1) + (fib n - 2) we peform the procedure which collapses
; to a new value. Basicly its just recursion but with several branching nodes, the "operation" gets peformed at the split
; points.
;
; Anyways i feel like the Chapter 1.2 tries to show me that we can use the actual computational structure as a computational
; tool. In ackermans function there are those "invisible intermediate steps" the process creates, that make the
; values expontiate so unintuitively, and they are generated by the underlying "structure" that the process makes, thus
; making calculating the end result from the input by hand so hard


; Counting change
; The books talks about a procedure that allows us to calculate all the ways we can
; make X amount of money out of Euros and Cents.
; I will attempt solving the problem on my own first of course, using this new "structural medium" as the tool to calculate with!

; Decoding the algorithm was surely much much harder than i expected, took me around 12 hours and AI assistance to get
; the logic down

; We are using set theory to split our "remaining amount" into all possible subsets of a set that contains
; all the possible tuples that can make our "remaining amount" --> S = {(a(5), b(10), c(20), d(50), e(100))...}
; So set S contains all of the possible combinations of values 5, 10, 20, 50, 100 to make our "remaining amount"
; We will then use two variables -> (amount options) to systematicly break down the set into all of its subsets.
; We will do this by limiting our options -> (set a rule like a = 0, or b = 0) OR by decrementing the "max integer"
; we can plug into a b c...   (a - 1(5), b(10).......)
; This allows us to systematicly create every possible subset of the original set!
; The base case for deciding which node of our tree is a unique combo, is a little fuzzy, but
; if our amount equals 0. it means we can make the remaining amount 0 exactly one way (with nothing!)
; If our available options hit 0, it means we dont have options to make our remaining amount!
; In other words if we hit a base case where (amount = 0) this means we have completed our tuple perfectly!
; From this we can see that what our algorithm does is either limiting options by setting them to equal 0
; Or by reducing the "current option" from our remaining amount  (a - 1(5)....)
; Lets just get going!
(define (count-change amount)
  (define (helper-options n)
    (cond
      [(= n 1) 5]
      [(= n 2) 10]
      [(= n 3) 20]
      [(= n 4) 50]
      [(= n 5) 100]
      ))
  (let loop (
             [amnt amount]
             [options 5]
             )
    (cond
      [(= amnt 0) 1]
      [(< amnt 0) 0]
      [(= options 0) 0]
      [else (+ (loop amnt (- options 1))
               (loop (- amnt (helper-options options)) options))]
      )))

; Now this algorithm does work, but its not the most efficient way to solve this problem
; We could desing a more efficient algorithm, using caching
; Basicly we would trade cpu cycles for memory, by saving each already solved function calls result into a cache

; Well here is one incompleted way to try to approach the problem, we are creating a list at every base case
; that contains (remaining-amount options running-sum)
; We then combine the sublists into one going up
; It falls down to the inefficiency of creating the list itself, and searching the list
; It mainly acts as a fancy way to visualize our algorithm in action though!
(define (count-change amount)
  (define (helper-options n)
    (cond
      [(= n 1) 5]
      [(= n 2) 10]
      [(= n 3) 20]
      [(= n 4) 50]
      [(= n 5) 100]
      ))
  (let loop (
             [amnt amount]
             [options 5]
             )
    (cond
      [(= amnt 0) (list 1 (list amnt options 1))]
      [(< amnt 0) (list 0 (list amnt options 0))]
      [(= options 0) (list 0 (list amnt options 0))]
      [else (let* (
                   [a-branch (loop amnt (- options 1))]
                   [b-branch (loop (- amnt (helper-options options)) options)]
                   [sum (+ (car a-branch) (car b-branch))]
                   [result (let list-loop (
                                             [a-list (cdr a-branch)]
                                             [b-list (cdr b-branch)]
                                             [new-list '()]
                                             )
                               (cond
                                 [(and (null? a-list) (null? b-list)) (cons sum (cons (list amnt options sum) new-list))]
                                 [(and (null? a-list) (not (null? b-list)))
                                  (list-loop a-list (cdr b-list) (cons (car b-list) new-list))]
                                 [(and (not (null? a-list)) (null? b-list))
                                  (list-loop (cdr a-list) b-list (cons (car a-list) new-list))]
                                 [else
                                   (list-loop (cdr a-list) (cdr b-list) (cons (car a-list) (cons (car b-list) new-list)))]
                                 ))]
                   ) result )])))

; Lets now try hash-tables instead
; We need to pass a mutable hash table along our whole process
(define (count-change amount)
  (define (helper-options n)
    (cond
      [(= n 1) 5]
      [(= n 2) 10]
      [(= n 3) 20]
      [(= n 4) 50]
      [(= n 5) 100]
      ))
  (let loop (
             [amnt amount]
             [options 5]
             )
    (cond
      [(= amnt 0) 1]
      [(< amnt 0) 0]
      [(= options 0) 0]
      [else (+ (loop amnt (- options 1))
               (loop (- amnt (helper-options options)) options))]
      )))




