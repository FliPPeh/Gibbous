local env = require "env"

env = env.new_environment()

print(env:eval[[
    (define (print txt)
        (. print txt))

    (define (yell txt)
        (print (. string.upper txt)))


    (let* ((a "yey")
           (b (. string.upper a)))
        (print b))
]])

