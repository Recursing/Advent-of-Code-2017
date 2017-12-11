#lang racket

(define input (string-split (string-trim (file->string "day11.txt")) ","))

(define sqrt3/2 1) ;whatever, it's just a scale factor

(define (add2 vec1 vec2)
  (cons (+ (car vec1) (car vec2))
        (+ (cdr vec1) (cdr vec2))))

; this adds directions from the end, no problems since + is commutative
(define (position-after path)
  (if (empty? path)
      (cons 0 0)
      (let ([direction (case (car path)
                         [("n") (cons -1 0)]
                         [("ne") (cons -1/2 sqrt3/2)]
                         [("se") (cons +1/2 sqrt3/2)]
                         [("s") (cons 1 0)]
                         [("sw") (cons +1/2 (- sqrt3/2))]
                         [("nw") (cons -1/2 (- sqrt3/2))])])
        (add2 direction
              (position-after (rest path))))))

(define (dist-from-origin pos)
  (let ([horiziontal-dist (abs (/ (cdr pos) sqrt3/2))])
    (+ horiziontal-dist
       (max 0 (- (abs (car pos))
                 (/ horiziontal-dist 2))))))

(dist-from-origin (position-after input))



(define (add-and-con list-pos dir)
  (cons (add2 (car list-pos) dir) list-pos))

; this adds positions from the end of the path, the history will be different!!!
(define (all-positions-after path)
  (if (empty? path)
      (cons (cons 0 0) '())
      (let ([direction (case (car path)
                         [("n") (cons -1 0)]
                         [("ne") (cons -1/2 sqrt3/2)]
                         [("se") (cons +1/2 sqrt3/2)]
                         [("s") (cons 1 0)]
                         [("sw") (cons +1/2 (- sqrt3/2))]
                         [("nw") (cons -1/2 (- sqrt3/2))])])
        (add-and-con (all-positions-after (cdr path)) direction))))

(apply max (map dist-from-origin (all-positions-after (reverse input))))
