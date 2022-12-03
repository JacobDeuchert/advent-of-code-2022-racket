#lang racket

(define (foldl-index f init lst)
    (foldl f init lst (range (length lst))))

(define read-file
    (port->string (open-input-file "./puzzle-data1.txt") #:close? #t))

(define (sum lst)
    (foldl + 0 lst))

(define (get-max-entry lst)
    (foldl-index (lambda (cur index prev) (
        if (> cur (car prev))
            (cons cur index)
            prev
    )) (cons 0 0) lst))

(define (sort-list lst)
    (sort lst (lambda (x y) (> x y))))

(define (parse-to-lists file-strg)
    (map (lambda (str) (string-split str "\n")) (string-split file-strg "\n\n")))

(define (sum-sub-lists list)
    (map (lambda (lst) (sum (map string->number lst))) list))


; Part 1
(car (get-max-entry (sum-sub-lists (parse-to-lists read-file))))

; Part 2
(sum (take (sort-list (sum-sub-lists (parse-to-lists read-file))) 3))



