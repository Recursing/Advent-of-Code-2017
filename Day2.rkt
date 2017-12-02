#lang racket

(define (split-parse line)
  (sort (map string->number (string-split line)) <))

(define mat
  (map split-parse (file->lines "day2.txt")))

(define (sum-diffs mat)
  (for/sum ([row mat])
    (- (apply max row) (apply min row))))

(define (divides? a b)
  (cond [(zero? a)  #f]
        [else (= (remainder b a) 0)]))

(define (find-multiple number row)
  (if (empty? row) #f
      (if (divides? number (car row))
          (/ (car row) number)
          (find-multiple number (cdr row)))))

(define (find-value row)
  (if (empty? row)
      (error "no pair found! D:")
      (or
       (find-multiple (car row) (cdr row))
       (find-value (cdr row)))))

(define (sum-divs mat)
  (for/sum ([row mat])
    (find-value row)))

(sum-diffs mat)
(sum-divs mat)
