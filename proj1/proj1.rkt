#lang plai-typed

;Problem 1
(define (sum numbers)
	(cond 
		[(empty? numbers) 0];If it's empty return 0 
		[else (+ (first numbers) (sum (rest numbers)))]))

"Problem 1 tests"
(test (sum(list)) 0)
(test (sum (list 1 1)) 2)
(test (sum (list 1 2 3)) 6)
(test (sum (list -1 -2 -3)) -6)


;Problem 2
(define (sumn numbers)
        (cond
		[(empty? numbers) 0];If it's empty return 0 
		[(> (first numbers) 0) (sumn(rest numbers))]
                [else (+ (first numbers) (sumn (rest numbers)))]))
"Problem 2 tests"
(test (sumn(list)) 0)
(test (sumn (list 1 -1)) -1)
(test (sumn (list -1 2 -3)) -4)
(test (sumn (list -1 -2 -3)) -6)
(test (sumn (list 1 2 3)) 0)


;Problem 3
(define (raise numbers)
	(map (lambda (num)
         	
		(cond   [(< num 0) 0]
			[else num]))
       		numbers))


"Problem 3 tests"
(test (raise (list 1 -1)) (list 1 0))
(test (raise (list -99 )) (list 0))
(test (raise (list 1 2 3 -4 -5 -6)) (list 1 2 3 0 0 0))
(test (raise (list 1 -2 3)) (list 1 0 3))


;Problem 4
(define (alternating lta)
	
)

(test (alternating (list 1 2 3 4 5)) (list 1 3 5))
