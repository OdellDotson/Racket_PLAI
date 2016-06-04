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


;;Problem 4
(define (alternating lta)
	(cond 	[(empty? lta) (list)]
		[else 
			(cond 	[(empty? lta) (list)]
				[(= (modulo (length lta) 2)0) (alternating (rest lta))]
				[else (cons (first lta) (alternating (rest lta)))])]))

"Problem 4 tests"
(test (alternating (list 1 2 3 4 5)) (list 1 3 5))
(test (alternating (list 1)) (list 1))
(test (alternating (list "Only" "Nan" " show" "NaN" " these" "NaN" " words.")) (list "Only" " show" " these" " words."))


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

(define gs3 (graduate "GS3" (scores 50 20 "Ungraded") "PhD"))
(define gs4 (graduate "GS4" (scores 80 80 "Ungraded") "PhD"))
(define gs5 (graduate "GS5" (scores 10 10 "Ungraded") "PhD"))

(test (all-phd-pass? (list gs1 gs2)) #t)
(test (all-phd-pass? (list gs1)) #t)
(test (all-phd-pass? (list ugs1 ugs2 gs1 gs2)) #t)
(test (all-phd-pass? (list gs3 gs4 gs5)) #f)


;;Problem 9

;;Returns a list broken on -999
(define (listBreaker lorf)
	(cond 	[(empty? lorf) (list)]
		[(= (first lorf) -999) (list)]
		[else (cons (first lorf)(listBreaker(rest lorf)))]))

;;Returns a list only of the positive numbers
(define (validList lorf)
	(cond 	[(empty? lorf) (list)]	
		[(< (first lorf) 0) (validList(rest lorf))]
		[else (cons(first lorf)(validList(rest lorf)))]))

;;Calcualtes avg. rainfall based on the rules of the problem
(define (rainfall lorf)
	(cond 	[(empty? lorf) 0]
		[(cons? lorf)	
			(/ (sum(validList (listBreaker lorf)))  (length (validList (listBreaker lorf))))]))



"Problem 9 Tests"

(test (listBreaker (list 1 2 -999 3)) (list 1 2))
(test (listBreaker (list 1 2 3)) (list 1 2 3))
(test (validList (list 1 2 -999 3)) (list 1 2 3))
(test (validList (listBreaker(list 1 2 -3 -4 -999 5))) (list 1 2))
(test (rainfall (list 1 2 3)) 2)
(test (rainfall (list 1 2 3 -999 4 5 6)) 2)
(test (rainfall (list 1 2 3 -4 -5 -6 -999 4 5 6 -8 9)) 2)


;; Problem 10
(define-type CartItem
     [item (name : string) (price : number)])   

(define (countPrices loi)
	(sum (map item-price loi)))


(define (isHat product)
	(string=? (item-name product) "hat" ))

(define (isShoe product)
	(string=? (item-name product) "shoes"))

(define (twoHats loi)
	(>=   (length (filter  isHat loi))   2))

(define (countShoePrice loi)
	(countPrices(filter isShoe loi)))

(define (lowerPricesShoes loi)
	(map (lambda (x) 
		(cond	[(isShoe x) (item (item-name x) (* (item-price x) .8))]
			[else x])) loi))

(define (checkout loi)
	(cond 	[(twoHats loi) 
			(- (cond[(>= (countShoePrice loi) 100) (countPrices (lowerPricesShoes loi))]
				[else (countPrices loi)])10)]
		[else(cond[(>= (countShoePrice loi) 100) (countPrices (lowerPricesShoes loi))]
                          [else (countPrices loi)]) ]))


"Problem 10 tests"
(define SC1 (list (item "shoes" 25) (item "bag" 50) (item "shoes" 85) (item "hat" 15)))
(define SC2 (list (item "hat" 20) (item "shoes" 10)))
(define SC3 (list (item "other" 25)))
(define SC4 (list (item "hat" 10) (item "hat" 20)))
(define SC5 (list (item "shoes" 100)))

(test (checkout SC1) 153);
(test (checkout SC2) 30);
(test (checkout SC3) 25);
(test (checkout SC4) 20);
(test (checkout SC5) 80);
