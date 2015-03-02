local env = require "scheme.env"

env = env.new_environment()

print(env:eval[[
    (define (yell txt)
        (print (. string.upper txt)))

    (let ((a "na")
          (b 8))
        (. print (. string.rep a b) "batman!"))

    (let ((a "a-val")
          (b "b-val"))
        (. print a b))

    (let* ((a "lagerregal")
           (b (. string.reverse a)))
        (. print b))
]])

