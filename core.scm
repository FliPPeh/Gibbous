(define (map fn ls)
  (if (null? ls)
        (list)
        (cons (fn (car ls))
              (map fn (cdr ls)))))

(define (fold-left fn acc ls)
  (if (null? ls)
        acc
        (let ((x  (car ls))
              (xs (cdr ls)))
            (fold-left fn (fn acc x) xs))))

(define (fold-right fn acc ls)
  (if (null? ls)
        acc
        (let ((x  (car ls))
              (xs (cdr ls)))
          (fn x (fold-right fn acc xs)))))

(define (for-each fn ls)
  (fold-left
    (lambda (acc x)
        (fn x))
    (list)
    ls))

(define (newline)
  (display #\newline))

(define (string . cs)
  (list->string cs))

