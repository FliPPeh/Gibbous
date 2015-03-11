local builtins = {}

local util = require "scheme.util"
local types = require "scheme.types"

local err = util.err
local expect = util.expect
local expect_argc = util.expect_argc
local expect_argc_min = util.expect_argc_min
local expect_argc_max = util.expect_argc_max


for _, mod in ipairs{
        require("scheme.builtins.comparison"),
        require("scheme.builtins.math"),
        require("scheme.builtins.list"),
        require("scheme.builtins.string"),
        require("scheme.builtins.typeconv"),
        require("scheme.builtins.io")} do

    for fname, fn in pairs(mod) do
        builtins[fname] = fn
    end
end


builtins["eval"] = function(self, env, args)
    expect_argc_min(self, 1, #args)
    expect_argc_max(self, 2, #args)

    if #args == 2 then
        util.not_implemented(args[2], "eval environment not yet supported")
    end

    return args[1]:eval(env)
end

builtins["apply"] = function(self, env, args)
    expect_argc_min(self, 1, #args)
    expect(args[1], "procedure")

    local fargs = {}

    for i = 2, #args do
        expect(args[i], "list")

        for j = 1, #args[i] do
            table.insert(fargs, args[i][j])
        end
    end

    return args[1]:call(env, fargs)
end

builtins["raise"] = function(self, env, args)
    expect_argc(self, 1, #args)

    print(self)
    util.err(self, "user-error", args[1])
end

return builtins
