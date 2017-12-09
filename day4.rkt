#lang racket

(define (contains? list element)
  (cond [(empty? list) #f]
        [(string=? element (car list)) #t]
        [else (contains? (cdr list) element)]))

(define (is-valid? phrase)
  (cond [(empty? phrase) #t]
        [(contains? (cdr phrase) (car phrase)) #f]
        [else (is-valid? (cdr phrase))]))

(define (solve phrase-list)
  (for/sum ([phrase phrase-list]
            #:when (is-valid? phrase))
    1))

(define phrase-list
  (map string-split (file->lines "day4.txt")))

(solve phrase-list)


(define (sorted-string string)
  (list->string (sort (string->list string) char<?)))

(define (sorted-words phrase)
  (map sorted-string phrase))

(solve (map sorted-words phrase-list))