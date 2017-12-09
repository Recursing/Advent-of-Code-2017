#lang racket

(define (total-score char-list level is-in-garbage)
  (let* ([first (car char-list)]
         [rest (cdr char-list)])
    (cond [(char=? first #\!) (total-score (cddr char-list) level is-in-garbage)]
          [(char=? first #\<) (total-score rest level #t)]
          [(char=? first #\>) (total-score rest level #f)]
          [is-in-garbage (total-score rest level #t)]
          [(char=? first #\{) (total-score rest (+ level 1) #f)]
          [(char=? first #\}) (+ level (total-score rest (- level 1) #f))]
          [(char=? first #\,) (total-score rest level #f)]
          [(char=? first #\newline) 0]
          [else (error "unexpected char!")])))

(total-score (string->list (file->string "day9.txt")) 0 #f)


(define (count-garbage char-list is-in-garbage)
  (let* ([first (car char-list)]
         [rest (cdr char-list)])
    (cond [(char=? first #\!) (count-garbage (cddr char-list) is-in-garbage)]
          [(char=? first #\>) (count-garbage rest #f)]
          [is-in-garbage (+ 1 (count-garbage rest #t))]
          [(char=? first #\<) (count-garbage rest #t)]
          [(char=? first #\newline) 0]
          [else (count-garbage rest #f)])))

(count-garbage (string->list (file->string "day9.txt")) #f)