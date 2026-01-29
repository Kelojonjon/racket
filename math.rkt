#lang racket/base
(require "lang/lang.rkt")
(provide (all-defined-out))



;; Racket usage instructions

;; ,enter "file.rkt"
;; ,bt
;; ,trace <func>
;; ,untrace <func>



;; Hello world!

(define (hello)
  (displayln "Hello, Math-World!"))


;; Constants

;; Speed of light
(define *c* 299792458)

;; Pi
(define pi 3.14159)



;; Math

;; Generate triangle numbers
(define (gauss_sum n)
  (/ (* n (+ n 1)) 2))

;; Return 1 if two factors produce both a triangle and a perfect square
(define (pells_equation x y)
  (let (
        [new_x (max x y)]
        [new_y (min x y)]
        )
    (- (* new_x new_x) (* 2(* new_y new_y)))))

;; Collect numbers in x...y range with a chosen stepsize
(define (math_collect x y step container)
  (cond
    [(>= y x)
     (let ([new_container (cons y container)])
        (math_collect x (- y step) step new_container))]
    [else container]))

;; Wrapper for math_collect
(define (collect x y [step 1])
  (let (
        [offset (modulo (abs (- x y)) step)]
        )(math_collect x (- y offset) step '())))

;; Repeat a custom-rule on any number for certain amount of steps,
(define (math_sequence x procedure steps container)
  (let (
        [new_x (procedure x)]
        )
  (cond
    [(> steps 0)
     (let ([new_container (cons new_x container)])
       (math_sequence new_x procedure (- steps 1) new_container))]
    [else (reverse container)])))

;; Wrapper for math_sequence
(define (sequence x procedure steps)
  (cons x (math_sequence x procedure steps '())))

;; Categorize integers in a range, into groups or same modulo-values
(define (math_modulo_chart x y mod steps container)
  (cond
    [(> steps 0)
     (let ([new_container (cons (collect x y mod) container)])
       (math_modulo_chart (+ x 1) y mod (- steps 1) new_container))]
    [else (reverse container)]))

;; Wrapper for math_modulo_chart
(define (modulo_chart x y mod)
  (math_modulo_chart x y mod mod '()))

;; Return the sum of all the values in the list
(define (math_list_sum sum container)
  (cond
    [(null? container) sum]
    [else (math_list_sum (+ sum (car container))(cdr container))]))

;; Wrapper for math_list_sum
(define (list_sum container)
  (math_list_sum 0 container))

;; Return the product of all the values in a list
(define (math_list_product product container)
  (cond
    [(null? container) product]
    [else (math_list_product (* product (car container))(cdr container))]))

;; Wrapper for math_list_product
(define (list_product container)
  (math_list_product 1 container))

;; Evaluate powers in form (num power)
(define (eval_power container)
  (expt (car container) (cadr container)))

;; Return the average of a list
(define (list_average container)
  (/ (list_sum container) (length container)))

;; The iterator for prime? predicant
(define (math_prime? num container)
  (cond
    [(null? container) #t]
    [(= 0 (modulo num (car container))) #f]
    [else (math_prime? num (cdr container))]))

;; Returns true if a number is a prime
(define (prime? num)
  (cond
    [(<= num 1) #f]
    [(and (not (= num 2)) (= 0 (modulo num 2))) #f]
    [else (let ([divisors (collect 3 (floor (sqrt num)) 2)])
            (math_prime? num divisors))]))

;; Returns all the primes in a given container
(define (math_list_primes container result) 
  (cond
    [(null? container) (reverse result)]
    [(prime? (car container))
     (let ([new_result (cons (car container) result)])
       (math_list_primes (cdr container) new_result))]
    [else (math_list_primes (cdr container) result)]))

;; Wrapper for math_list_primes
(define (list_primes container)
  (math_list_primes container '()))

;; Factorize a number into its prime numbers
(define (math_factor num x container)
  (cond
    [(> (* x x) num) (cons num container)]
    [(= 0 (modulo num x)) (math_factor (/ num x) x  (cons x container))]
    [else (math_factor num (+ x 1)  container)]))

;; Wrapper for math_factor
(define (factor num)
  (math_factor num 2 '()))

;; Simplify a list of numbers into (num amount) for each number
;; Works with other data, like strings also!
(define (math_simplify_factors x amount container next_container result)
  (cond
    [(null? container)
     (if (null? next_container)
         (cons (list x amount) result) 
         (math_simplify_factors #f 0 next_container '() (cons (list x amount) result)))]
    [(false? x) 
     (math_simplify_factors (car container) 0 container '() result)]
    [(equal? x (car container)) 
     (math_simplify_factors x (+ 1 amount) (cdr container) next_container result)]
    [else
     (math_simplify_factors x amount (cdr container) (cons (car container) next_container) result)]))

;; Wrapper for simplify factors
(define (simplify_factors container)
  (math_simplify_factors #f 0 container '() '()))

;; Return a list of common factors between 2 numbers in from ((num pwr) (num pwr))
(define (math_common_factors container_a container_b container_c result)
  (cond
    [(null? container_a) result]
    [(null? container_b) (math_common_factors (cdr container_a) container_c container_c result)]
    [(= (car (car container_a)) (car (car container_b)))
     (if (> (cadr (car container_a)) (cadr (car container_b)))
         (math_common_factors (cdr container_a) container_c container_c (cons (car container_b) result))
         (math_common_factors (cdr container_a) container_c container_c (cons (car container_a) result)))]
    [else (math_common_factors container_a (cdr container_b) container_c result)]))

;; Wrapper for math_common_factors
(define (common_factors num_a num_b)
  (let (
        [factors_a (simplify_factors (factor num_a))]
        [factors_b (simplify_factors (factor num_b))]
        )
    (math_common_factors factors_a factors_b factors_b '())))

;; Simplify a ratio 
(define (simplify_ratio num_a num_b)
  (let* (
        [shared_factors (map eval_power (common_factors num_a num_b))]
        [g_cd (list_product shared_factors)]
        [new_a (/ num_a g_cd)]
        [new_b (/ num_b g_cd)]
        )(list new_a new_b)))

;; Collect 2 points from both sides of a point, from a set distance away
(define (center_interval center diameter)
  (let* (
        [radius (/ diameter 2)]
        [point_a (- center radius)]
        [point_b (+ center radius)] 
        )(list point_a point_b)))

;; Find the percentage change on the terms of old-value
(define (percentage_change new_value old_value)
  (abs (- 100 (* 100 (/ new_value old_value)))))

;; Absolute distance between 2 points
(define (distance_between x y)
  (abs (- x y)))

;; Middlepoint of two points
(define (middlepoint x y)
  (/ (+ x y) 2))

;; Returns the factorial of num
(define (math_factorial num product)
  (cond
    [(> num 1) (math_factorial (- num 1) (* product (- num 1)))]
    [else product]))

;; Wrapper for math_factorial
(define (factorial num)
  (math_factorial num num))

;; Binary search for square root of the "target"
;; Search range "x".."y"
;; Precicion control with "tol_low" "tol_upp" from the target number
(define (math_binary_sqrt target x y tol_low tol_upp)
  (let* (
         [mid (middlepoint x y)]
         [curr_sqrt (* mid mid)]
         )
    (cond
      [(< y x) #f]
      [(= mid x) mid]
      [(= mid y) mid]
      [(and (> curr_sqrt tol_low)(< curr_sqrt tol_upp)) mid]
      [(> target curr_sqrt) (math_binary_sqrt target mid y tol_low tol_upp)]
      [(< target curr_sqrt) (math_binary_sqrt target x mid tol_low tol_upp)]
      [else mid])))

(define (binary_sqrt_dispatch target x y tolerance)
  (let* (
         [tol_val (center_interval target tolerance)]
         [tol_low (car tol_val)]
         [tol_upp (cadr tol_val)]
         )
    (cond
      [(<= target 0) #f]
      [else (math_binary_sqrt target x y tol_low tol_upp)])))

;; Newtons method for finding the square root
(define (math_newton_sqrt target x tol_low tol_up)
  (let* (
        [new_x (/ (+ x (/ target x)) 2)]
        [square (* x x)]
        )
    (cond
      [(and (>= square tol_low) (<= square tol_up)) x]
      [else (math_newton_sqrt target new_x tol_low tol_up)])))

;; The wrapper for the math_newton_sqrt
;; x for initial guess, tolerance for accuracy
(define (newton_sqrt target x tol)
  (let* (
        [tolerance (center_interval target tol)]
        [tol_low (car tolerance)]
        [tol_up (car (cdr tolerance))]
        )(math_newton_sqrt target x tol_low tol_up)))

;; Turn a number into binary format
(define (math_num->bin number container)
  (cond
    [(not (= number 0)) (let 
       ([new_container (cons (remainder number 2) container)])
          (math_num->bin (quotient number 2) new_container))]
    [else container]))

;; Wrapper for math_to_binary that handles the 0 case
(define (num->bin number)
  (cond
    [(= number 0) '(0)]
    [else (math_num->bin number '() )]))

;; Number to binary usiing horners method (whiich is superior to my approach)
;; Number must be in a list format
(define (bin->num bin res)
  (cond 
    [(null? bin) res]
    [(= 1 (car bin)) (bin->num (cdr bin) (+ (* res  2) 1))]
    [else (bin->num (cdr bin) (* res 2))]))

;; Takes a positive integer of any size and turns it into a list
(define (math_num->list number container)
  (let (
        [remd (remainder number 10)]
        [quot (quotient number 10)]
        )
    (cond
      [(<= quot 0) (cons remd container)]
      [else
        (let ([new_container (cons remd container)])
          (math_num->list quot new_container))])))

;; Wrapper for math_to_num_list
;; Takes integer and fraction part separately
;; Example (num_to_list 1234 5678 0) -> ((1 2 3 4) (3 6 7 8))
;; Select the amount of leading zeroes in the fraction with "frac_pad"
;; Example (num_to_list 5 007 2) -> ((5) (0 0 7))
(define (num->list int frac frac_pad)
  (let* (
         [abs_int (abs int)]
         [abs_frac (abs frac)]
         [quot (math_num->list abs_int '())]
         [remd (math_num->list abs_frac '())]
         [pad_remd (pad_list 0 frac_pad remd)]
         )(list quot pad_remd)))

;; Pad a list with any data select amount of time
(define (pad_list data amount container)
  (cond
    [(<= amount 0) container]
    [else
      (let ([new_container (cons data container)])
        (pad_list data (- amount 1) new_container))]))

;; Area of a circle
(define (area_circle radius)
  (* pi (expt radius 2)))

;; Area of the band between 2 circles
(define (area_circles_band x y)
  (abs (- (area_circle x) (area_circle y))))

; Area of a perfect square
(define (area_square x)
  (* x x))

; Volume of a cube
(define (volume_cube x y z)
  (* x y z))


