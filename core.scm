; Basic stuff
(define nil ())

; Functional stuff
(define (map fn ls)
  (if (null? ls)
        ()
        (cons (fn (car ls))
              (map fn (cdr ls)))))

(define (filter fn ls)
   (if (null? ls)
         ()
         (let ((x  (car ls))
               (xs (cdr ls)))
             (if (fn x)
                 (cons x (filter fn xs))
                 (filter fn xs)))))

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
    ()
    ls))

; Embarassing stuff
(define (call/cc f)
  (raise "call/cc not implemented"))

(define call-with-current-continuation call/cc)

; I/O stuff
(define (call-with-input-file fnam fn)
  (let* ((f   (open-input-file fnam))
         (res (fn f)))
    (close-input-port f)
    res))

(define (call-with-output-file fnam fn)
  (let* ((f   (open-output-file fnam))
         (res (fn f)))
    (close-output-port f)
    res))

; Char stuff
(define (char-lower-case? c)
  (and (char>=? c #\a)
       (char<=? c #\z)))

(define (char-upper-case? c)
  (and (char>=? c #\A)
       (char<=? c #\Z)))

(define (char-alphabetic? c)
  (or (char-lower-case? c)
      (char-upper-case? c)))

(define (char-numeric? c)
  (and (char>=? c #\0)
       (char<=? c #\9)))

(define (char-whitespace? c)
  (or (char=? c #\x0A)
      (char=? c #\x0D)
      (char=? c #\x09)
      (char=? c #\x20)))


(define (char-downcase c)
  (if (char-upper-case? c)
    (integer->char (bit32.bor 32 (char->integer c)))
    c))

(define (char-upcase c)
  (if (char-lower-case? c)
    (integer->char (bit32.band 223 (char->integer c)))
    c))

; String stuff
(define (string . cs)
  (list->string cs))

(define (make-string n c)
  (string.rep c n))

(define (string-downcase str)
  (list->string (map char-downcase (string->list str))))

(define (string-upcase str)
  (list->string (map char-upcase (string->list str))))

(define (string-copy str)
  (substring str 0))

; Map stuff
(define (map-get* m . keys)
  (if (null? keys)
    m
    (apply map-get*
      (list (map-get m (car keys)))
      (cdr keys))))

; List stuff
(define (list . es)
  es)

(define (caar x)    (car (car x)))
(define (cadr x)    (car (cdr x)))
(define (cdar x)    (cdr (car x)))
(define (cddr x)    (cdr (cdr x)))
(define (caaar x)   (car (car (car x))))
(define (caadr x)   (car (car (cdr x))))
(define (cadar x)   (car (cdr (car x))))
(define (caddr x)   (car (cdr (cdr x))))
(define (cdaar x)   (cdr (car (car x))))
(define (cdadr x)   (cdr (car (cdr x))))
(define (cddar x)   (cdr (cdr (car x))))
(define (cdddr x)   (cdr (cdr (cdr x))))
(define (caaaar x)  (car (car (car (car x)))))
(define (caaadr x)  (car (car (car (cdr x)))))
(define (caadar x)  (car (car (cdr (car x)))))
(define (caaddr x)  (car (car (cdr (cdr x)))))
(define (cadaar x)  (car (cdr (car (car x)))))
(define (cadadr x)  (car (cdr (car (cdr x)))))
(define (caddar x)  (car (cdr (cdr (car x)))))
(define (cadddr x)  (car (cdr (cdr (cdr x)))))
(define (cdaaar x)  (cdr (car (car (car x)))))
(define (cdaadr x)  (cdr (car (car (cdr x)))))
(define (cdadar x)  (cdr (car (cdr (car x)))))
(define (cdaddr x)  (cdr (car (cdr (cdr x)))))
(define (cddaar x)  (cdr (cdr (car (car x)))))
(define (cddadr x)  (cdr (cdr (car (cdr x)))))
(define (cdddar x)  (cdr (cdr (cdr (car x)))))
(define (cddddr x)  (cdr (cdr (cdr (cdr x)))))

(define (append xs ys)
  (fold-right cons ys xs))

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

(define (positive? x)
  (>= x 0))

(define (negative? x)
  (not (positive? x)))
