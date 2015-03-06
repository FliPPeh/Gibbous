local env = require "scheme.env"

env = env.new_environment()
env:eval_file("core.scm")

res = env:eval(arg[1] or io.read('*a'))

print((">> %s\n  %s"):format(res, res:type()))

