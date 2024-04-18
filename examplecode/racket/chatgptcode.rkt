#lang racket
(define (memberAt i lis)
  (if (= i 0)
      (car lis)
      (memberAt (- i 1) (cdr lis))))

(define (avgOfMiddle2 lis)
  (let
    [(halfLisLength (floor (/ (length lis) 2)))]
    (/ (+ (memberAt (- halfLisLength 1) lis)
          (memberAt halfLisLength lis))
       2)))

(define (middle1 lis)
  (memberAt (/ (length lis) 2) lis))

(define (median lis)
  (let*
      [(sortedlis1 (sort lis <))]
      (if (even? (length lis)) (avgOfMiddle2 sortedlis1) (middle1 sortedlis1))))
