#lang racket

; mean, median and mode n stuff

(define (findMax lis)
  (if (= (length lis) 1)
      (car lis)
      (if (> (car lis) (findMax (cdr lis)))
          (car lis)
          (findMax (cdr lis)))))

(define (mean lis)
  (if (= (length lis) 0) (print "Hey, you can't do that")
  (/ (foldl + 0 lis) (length lis))))

(define (median lis)
  (let*
      [
       (sortedlis1 (sort lis <))
      ]
    (if (= (length lis) 0) (print "Hey, you can't do that either!")
      (if (even? (length lis)) (avgOfMiddle2 sortedlis1) (middle1 sortedlis1)))))

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
  (/ (+ (memberAt (- halfLisLength 1) lis) (memberAt halfLisLength lis)) 2)))

(define (middle1 lis)
  (memberAt (- (floor (/ (length lis) 2)) 1) lis))

(define (countAppearances atm lis)
  (cond
    [(empty? lis) 0]
    [(equal? atm (car lis)) (+ 1 (countAppearances atm (cdr lis)))]
    [else (countAppearances atm (cdr lis))]))

(define (countAll lis1 lis2)
   
  (if (= (length lis1) 1)
    (list (cons (car lis1) (countAppearances (car lis1) lis2)))
    (append (list (cons (car lis1) (countAppearances (car lis1) lis2))) (countAll (cdr lis1) lis2))))

(define (mode lis)
  (let
      [
       (listcounts (countAll (remove-duplicates lis) lis))
      ]
    (car (car (sort listcounts pairorder)))))

(define (pairorder a b)
  (> (cdr a) (cdr b)))

