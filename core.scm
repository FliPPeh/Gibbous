; Functional stuff
(define nil ())

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

; I/O stuff
(define (newline)
  (display #\newline))

; Type stuff
(define (string . cs)
  (list->string cs))

(define (list . es)
  es)

; Math stuff
(define (sin x) (math.sin x))
(define (cos x) (math.cos x))
(define (tan x) (math.tan x))
(define (asin x) (math.asin x))
(define (acos x) (math.acos x))
(define (atan x) (math.atan x))

(define (sinh x) (math.sinh x))
(define (cosh x) (math.cosh x))
(define (tanh x) (math.tanh x))

(define (sqrt n) (math.sqrt n))
(define (expt base n) (math.pow base n))
(define (log base n) (math.log base n))

(define (floor x) (math.floor x))
(define (ceiling x) (math.ceil x))
(define (truncate x)
  (if (> x 0)
    (floor x)
    (- (floor (- x)))))

(define (round x)
  (if (>= (- x (floor x)) 0.5)
    (+ (floor x) 1)
    (floor x)))

(define (abs x)
  (if (< x 0)
    (- x)
    x))
