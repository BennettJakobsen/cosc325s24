#lang racket

; mean, median and mode n stuff

(define (findMax lis)
  (if (= (length lis) 1)
      (car lis)
      (if (> (car lis) (findMax (cdr lis)))
          (car lis)
          (findMax (cdr lis)))))


(define (findMaxc lis)
  (cond
    [(= (length lis) 1) (car lis)]
    [(> (car lis) (findMaxc (cdr lis))) (car lis)]
    [else (findMaxc (cdr lis))]
    ))

(define (memberAt i lis)
  (if (= i 0)
      (car lis)
      (memberAt (- i 1) (cdr lis))))

(define (avgOfMiddle2 lis)
  (let
    [(halfLisLength (floor (/ (length lis) 2)))]
  ; car of lis and then reverse and car again, continue until = length lis 2?
  (/ (memberAt (+ halfLisLength 1) lis) (memberAt halfLisLength lis))))

(define (middle1 lis)
  (memberAt (floor (/ (length lis) 2)) lis))
  

(define (median lis)
  (let
      [(sortedlis (sort lis <))]
    (if (even? (length lis)) (avgOfMiddle2 lis) (middle1 lis))))