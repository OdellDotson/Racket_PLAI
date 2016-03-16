#lang plai-typed

;Problem 1
(define (sum numbers)
	(cond 
		[(empty? numbers) 0];If it's empty return 0 
		[else (+ (first numbers) (sum (rest numbers)))]))

(test (sum (list 1 1)) 2)
(test (sum (list 1 2 3)) 6)
(test (sum (list -1 -2 -3)) -6)



;Problem 2
(define (sumnegatives numbers)
        (cond
		[(positive? 5) (error "doesn't get here")]
                ;[(positive? (first numbers)) (sum(rest numbers))]
		[(empty? numbers) 0];If it's empty return 0 
                [else (+ (first numbers) (sum (rest numbers)))]))

(test (sumnegatives (list 1 -1)) -1)
(test (sumnegatives (list -1 2 -3)) -4)
(test (sumnegatives (list -1 -2 -3)) -6)
(test (sumnegatives (list 1 2 3)) 0)

