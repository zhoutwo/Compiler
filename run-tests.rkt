#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "compiler.rkt")
(require "passes/type-check.rkt")

(debug-level 4)

(compiler-tests "compiler.rkt" type-check r5-passes "r0" (make-list 4))
(compiler-tests "compiler.rkt" type-check r5-passes "r1" (make-list 48))
(compiler-tests "compiler.rkt" type-check r5-passes "r1a" (make-list 8))
(compiler-tests "compiler.rkt" type-check r5-passes "r2" (make-list 59))
(compiler-tests "compiler.rkt" type-check r5-passes "r3" (make-list 35))
(compiler-tests "compiler.rkt" type-check r5-passes "r4" (make-list 38))
(newline) (display "tests passed!") (newline)
