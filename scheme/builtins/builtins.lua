local builtins = {}

local util = require "scheme.util"
local types = require "scheme.types"

local desymbolize = util.desymbolize
local err = util.err
local ensure = util.ensure
local expect = util.expect
local expect_argc = util.expect_argc
local expect_argc_min = util.expect_argc_min
local expect_argc_max = util.expect_argc_max


for _, mod in ipairs{
        require("scheme.builtins.comparison"),
        require("scheme.builtins.math"),
        require("scheme.builtins.list"),
        require("scheme.builtins.map"),
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

    return desymbolize(args[1]):eval(env)
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
    expect_argc_min(self, 1, #args)

    if #args == 1 then
        expect(args[1], "string")
        util.err(self, "user-error", args[1])
    else
        expect(args[1], "string")
        expect(args[2], "string")

        ensure(args[1], args[1]:getval():find("[^%w%-]") == nil,
            "argument-error",
            "error kind may only contain alphanumerics and hyphens")

        util.err(self, args[1], args[2])
    end
end

builtins["error-info"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "error")

    return types.pair.new(
        types.toscheme(args[1].errtype),
        types.toscheme(args[1].errmsg))
end

return builtins
