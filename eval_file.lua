local env = require "scheme.env"

env = env.new_environment()
env:eval_file("core.scm")

print(env:eval_file(arg[1]))

