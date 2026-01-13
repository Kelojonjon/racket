#lang racket/base
(require "core.rkt")
(provide (all-defined-out))

;; Wrappers for language specific stuff

(define false? core_false?)

(define missing! core_missing!)

(define (index depth container)
  (core_index_wrap depth container))

(define (type_check types . args)
  (core_type_check types args))

