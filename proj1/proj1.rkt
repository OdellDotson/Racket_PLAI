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
;(define (alternating lta)
;	
;)

;(test (alternating (list 1 2 3 4 5)) (list 1 3 5))


;; Problem 5

(define-type Scores
	[scores (midtermGrade : number) (finalGrade : number) (courseGrade : string)])

;; Problem 6
(define-type Students
	[undergrad (name : string) (grades : Scores) (gradYear : number)]
	[graduate (name : string) (grades : Scores) (degreeProg : string)])

;; (Problem 7 : spaghetti)
(define (assign-grades los)
	(cond   [(empty? los) empty]
		[(cons? los) 
			(map (lambda (student) 
				(cond   [(undergrad? student) 
						(undergrad (undergrad-name student) (scores (scores-midtermGrade (undergrad-grades student)) (scores-finalGrade (undergrad-grades student))
						(cond 	[(> (/ (+ (scores-midtermGrade (undergrad-grades student)) (scores-finalGrade (undergrad-grades student)) ) 2) 85) "high pass"]
							[(> (/ (+ (scores-midtermGrade (undergrad-grades student)) (scores-finalGrade (undergrad-grades student)) ) 2) 65) "pass"]
							[else "fail"]
							)) (undergrad-gradYear student))]
					[(graduate? student) 
						(graduate (graduate-name student) (scores (scores-midtermGrade (graduate-grades student)) (scores-finalGrade (graduate-grades student)) 
						(cond [(> (/ (+ (scores-midtermGrade (graduate-grades student)) (scores-finalGrade (graduate-grades student))) 2) 85) "high pass"]
                                                      [(> (/ (+ (scores-midtermGrade (graduate-grades student)) (scores-finalGrade (graduate-grades student))) 2) 65) "pass"]
                                                      [else "fail"])) (graduate-degreeProg student))])
			)
			los)]))
"Problem 7 Tests"	
(define ugs1 (undergrad "UGS1" (scores 90 95 "A") 2017))
(define ugs2 (undergrad "UGS2" (scores 85 80 "B") 2019))
(define gs1 (graduate "GS1" (scores 70 75 "C") "PhD"))
(define gs2 (graduate "GS2" (scores 35 40 "NR") "MS"))

(define tugs1 (undergrad "UGS1" (scores 90 95 "high pass") 2017))
(define tugs2 (undergrad "UGS2" (scores 85 80 "pass") 2019))
(define tgs1 (graduate "GS1" (scores 70 75 "pass") "PhD"))
(define tgs2 (graduate "GS2" (scores 35 40 "fail") "MS"))



(test (assign-grades (list ugs1 ugs2 gs1 gs2)) (list tugs1 tugs2 tgs1 tgs2))  



;; Problem 8
(define (all-phd-pass? los)
	(foldl (lambda(a b) (and a b)) #t (map (lambda (student)
		(cond [(empty? los) #t];If there's no one on the list, everyone on the list passed.
			[(cons? los) 
				(cond [(undergrad? student) #t]
					[(graduate? student) 
						(cond [(string=? "MS" (graduate-degreeProg student)) #t];true
							[ (string=? "PhD" (graduate-degreeProg student)) 
							(cond [(string=? "pass" (scores-courseGrade (graduate-grades student)))#t]
								[else #f])	
								])])])) (assign-grades los) )));false

"Problem 8 Tests"
(test (all-phd-pass? (list gs1 gs2)) #t)
(test (all-phd-pass? (list gs1)) #t)




