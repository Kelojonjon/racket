#lang racket
(provide (all-defined-out))
(require racket/trace)

; Collection of codewards stuff

;(provide nb-year)

; Calculate the population increase per year with the following variables
; p0 = starting population
; percent = growth per yer in percents
; aug = added linear growth for example 50 people / year
; p = treshold population, when we exeed this return the amount of years needed to get here
(define (nb-year p0 percent aug p)
  ; Since we cant loop the "mother function" in codewars we are gonna create a looping
  ; function or something similar inside the "mother function"
  (let loop (
             [popl p0]
             [year 0]
             )
    (let (
           [incr (floor (+ popl aug (* (/ popl 100) percent)))]
           )
      (cond 
        [(and (= year 0) (>= incr p)) 0]
        [(>= incr p) (+ year 1)]
        [else (loop incr (+ year 1))]
        ))))

(define (max-rot1 n)
  (if (= 0 n) 0
      (let* (
             [n-len (let loop (
                               [power 1]
                               [len 0]
                               )
                      (if (< 1 (/ power n)) len
                          (loop (* 10 power) (+ 1 len))))]
             [denom (expt 10 (- n-len 1))]
             [sep-num (let loop (
                                 [deno denom]
                                 [nume n]
                                 [result '()]
                                 )
                        (if (< deno 1) result
                            (let (
                                  [quot (quotient nume deno)]
                                  [rem (remainder nume deno)]
                                  )
                              (loop (/ deno 10) rem (cons quot result)))))]
             ) sep-num)))

(define (max-rot2 n)
  (if (< n 10) n
      (let (
            [n-pwr (let loop (
                              [power 1]
                              [len 0]
                              )
                     (if (< 1 (/ power n)) (expt 10 (- len 1))
                     (loop (* 10 power) (+ 1 len))))]
            )
        (let loop (
                   [result '()]
                   [num n]
                   [power n-pwr]
                   [max-sum n]
                   )
          (if (= 1 power) max-sum
              (let* (
                     [pwr-sum (let sum-loop (
                                             [curr-pwr n-pwr]
                                             [powers (reverse result)]
                                             [sum 0]
                                             )
                                (cond 
                                  [(null? powers) sum]
                                  [else (sum-loop (/ curr-pwr 10) (cdr powers)
                                                  (+ sum (* curr-pwr (car powers))))]))]
                     [int-res (+ pwr-sum num)]
                     )
                 (let* (
                        [head (quotient num power)]
                        [tail (remainder num power)]
                        [next-power (/ power 10)]
                        [extract (quotient tail next-power)]
                        [new-num (+ head (* 10 (remainder tail next-power)))]
                        )
                   (loop (cons extract result) new-num next-power
                         (if (> int-res max-sum) int-res max-sum)))))))))





    

