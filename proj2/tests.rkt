#lang plai-typed

; this is a sample test suite

(define p1 '(+ 3 4))

(test '(+ 2 2) (numV 4)) ; Here we test if a basic addition returns the correctly typed numeric value.
(test (run p1) (numV 7)) ; Testing if a simple function definition works
(test/exn (run '(+ x 4)) "unbound") ; Testing if ???
(test '(* 3 3) *numV 9) ; Testing multiplication
(test '(with ((x 7)) (x)) (numV 7)) ; Testing basic WITH functionality
(test (with ((x 3) (y 5))(* x y)) (numV 15)) ; Testing the WITH functionalilty used in a funcrion.

