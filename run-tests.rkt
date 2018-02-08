#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "compiler.rkt")
(require "passes/type-check.rkt")

(debug-level 4)

(compiler-tests "compiler.rkt" type-check r4-passes "r0" (make-list 4))
(compiler-tests "compiler.rkt" type-check r4-passes "r1" (make-list 48))
(compiler-tests "compiler.rkt" type-check r4-passes "r1a" (make-list 8))
(compiler-tests "compiler.rkt" type-check r4-passes "r2" (make-list 59))
(compiler-tests "compiler.rkt" type-check r4-passes "r3" (make-list 35))
(compiler-tests "compiler.rkt" type-check r4-passes "r4" (make-list 34))
;(compiler-tests "compiler.rkt" type-check r4-passes "r4" (list 35))
(compiler-tests "compiler.rkt" type-check r4-passes "r4" (list 36 37 38))
(newline) (display "tests passed!") (newline)
