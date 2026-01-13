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



;; Math

;; Collect a list of numbers from x..y
(define (math_collect x y container)
  (cond
    [(> y (- x 1))
     (let ([new_container (cons y container)]) ;; Create a "new version of the old var with the new y added" with linked lists
       (math_collect x (- y 1) new_container))]
    [else container]))

;; Wrapper for math_collect
(define (collect x y)
  (math_collect x y '()))

;; Collect 2 points from both sides of a point, from a set distance away
(define (center_interval center radius)
  (let* (
         [pnt_low (- center radius)]
         [pnt_upp (+ center radius)]
         )
    (list pnt_low pnt_upp)))

;; Absolute distance between 2 points
(define (distance_between x y)
  (abs (- x y)))

;; Middlepoint of two points
(define (middlepoint x y)
  (/ (+ x y) 2))

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

;; Turn a binary number to decimal format
;; Number needs to be in a list format
(define (bin_to_num bin res x)
  (cond
    [(null? bin) res]
    [(and (= (car bin) 1)(= x 0))
       (bin_to_num (cdr bin) (+ res (expt 2 x)) (- x 1))]
    [(= (car bin) 1) 
       (bin_to_num (cdr bin) (+ res (expt 2 x)) (- x 1))]
    [else (bin_to_num (cdr bin) res (- x 1))]))

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
    
