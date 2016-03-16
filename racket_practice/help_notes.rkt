#lang plai-typed
;333fred is his github
;(define (string-list-length-fold los)
;	(foldl (lambda (str acc) (+ (string_length str) acc)) 0 los))
;(define (concat-list-fold los)
;	(fold1 (lambda (str acc) (string-append acc str)) "" los))

;(test (string-list-length-fold empty) 0)
;(test (concat-list-fold (list "fred" "ted")) "fredted")



(define (string-list-length-fold-map los)
	(foldl + 0 
		(filter (lambda (str-len) (> str-len 3)) 
			(map string-length los))))


;structs? 

(define-type Room
	[lecture-hall (room-num:number) (cap : number) (chair-type:string)];All need to be specified out just like this one.
	[computer-lab num-comps room-num computer-type]
	[classroom room-num cap board-type]
	[office room-num professor]
)

(define kfisler-office (office 130 "Kathi"))
(define fl320 (classroom 320 60 "whiteboard"))
(define sl15 (lecture-hall 115 120 "uncomfortable"))
(define sl120 (computer-lab 30 120 "dell"))


(define (get-room-nums lor)
	(cond   [(empty? lor) empty]
		[(cons? lor) 
			(cons (let ([first-room (first lor)])
				(cond [(lecture-hall? first-room) (lecture-hall-room-num first-room)
					[(computer-lab? first room) (computer-lab-room-num first-room)
					[ contnued conditions for all go here])) 
				(get-room-nums (rest lor)))]))






;(test (string-list-length-fold-map
