#lang racket/base
(provide (all-defined-out))

;; CORE BEHAVIOUR

(define core_false? not)

(define core_missing! (string->symbol "!!missing!!"))

;; Indexing for lists. 0-index means the last item in the list
;; Examples
;; index (1,2,3,4,5)  3 -> 1
;; index (1,2,3,4,5)  0 -> 5
;; index (1,2,3,4,5) -1 -> 4
(define (core_index_wrap depth container)
  (let (
        [len (length container)]
        )
    (cond
      [(not (real? depth)) (error "INDEX CRASH: depth must be a integer!")]
      [(not (pair? container)) (error "INDEX CRASH: container is not a list!")]
      [(> (abs depth) len) core_missing!]
      [(and (> (abs (- 1 depth)) len) (<= depth 0)) core_missing!]
      [(<= depth 0) (core_index_walk (- len (abs depth)) container)]
      [(> depth 0) (core_index_walk depth container)])))

(define (core_index_walk depth container)
    (cond
      [(> depth 1) (core_index_walk (- depth 1) (cdr container))]
      [(= depth 1) (car container)]))

;; Take a list, and return true if all arguments belong to the allowed types
(define (core_type_check allowed_types . args)
  (print"HELLO"))
  
