local m = {}

local util = require "scheme.util"
local types = require "scheme.types"

local ensure = util.ensure
local expect = util.expect
local expect_argc = util.expect_argc
local expect_argc_min = util.expect_argc_min
local expect_argc_max = util.expect_argc_max

local number_new = types.number.new
local char_new = types.char.new
local str_new = types.str.new
local bool_new = types.boolean.new

--[[
-- String stuff
--]]
m["to-string"] = function(self, env, args)
    expect_argc(self, 1, #args)

    return str_new(tostring(args[1]))
end

-- TODO: Scheme formatting
m["format"] = function(self, env, args)
    expect_argc_min(self, 1, #args)

    local fargs = {}
    for i = 2, #args do
        if args[i].type ~= "string" and args[i].type ~= "number" then
            table.insert(fargs, tostring(args[i]))
        else
            table.insert(fargs, args[i]:getval())
        end
    end

    return str_new(string.format(args[1]:getval(), table.unpack(fargs)))
end


m["string-length"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return number_new(#args[1]:getval())
end

m["string-null?"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return bool_new(#args[1]:getval() == 0)
end

m["string-append"] = function(self, env, args)
    local res = ""

    for _, str in ipairs(args) do
        expect(str, "string")

        res = res .. str:getval()
    end

    return str_new(res)
end

m["string-join"] = function(self, env, args)
    expect_argc_min(self, 1, #args)
    expect_argc_max(self, 2, #args)

    expect(args[1], "list")

    local res = {}
    local sep = " "

    if #args == 2 then
        expect(args[2], "string")

        sep = args[2]:getval()
    end

    for _, str in ipairs(args[1]:getval()) do
        expect(str, "string")

        table.insert(res, str:getval())
    end

    return str_new(table.concat(res, sep))
end

m["string-split"] = function(self, env, args)
    expect_argc_min(self, 1, #args)
    expect(args[1], "string")

    local sep = "%s"

    if #args == 2 then
        expect(args[2], "string")

        sep = util.literal_pattern(args[2]:getval())
    end

    return types.toscheme(util.split_string(args[1]:getval(), sep))
end

m["string-trim"] = function(self, env, args)
    expect_argc_min(self, 1, #args)
    expect(args[1], "string")

    local sep_a, sep_b = "^%s*", "%s*$"

    if #args == 2 then
        expect(args[2], "string")

        sep_a = "^" .. util.literal_pattern(args[2]:getval())
        sep_b = util.literal_pattern(args[2]:getval()) .. "$"
    end

    return str_new(args[1]:getval():gsub(sep_a, ""):gsub(sep_b, ""))
end


m["string-ref"] = function(self, env, args)
    expect_argc(self, 2, #args)
    expect(args[1], "string")
    expect(args[2], "number")
    ensure(args[2], (args[2]:getval() + 1) <= #args[1]:getval(),
        "argument-error",
        "string index out of bounds")

    local ind = args[2]:getval() + 1

    return char_new(args[1]:getval():sub(ind, ind))
end

m["substring"] = function(self, env, args)
    expect_argc_min(self, 2, #args)
    expect(args[1], "string")
    expect(args[2], "number")

    local str = args[1]:getval()
    local sstart, send = args[2]:getval(), #str

    ensure(args[2], sstart <= #str,
        "argument-error",
        "string index out of bounds")

    if #args == 3 then
        expect(args[3], "number")

        send = args[3]:getval()

        ensure(args[3], send >= sstart,
            "argument-error",
            "end must be greater or equal to start")

        ensure(args[3], sstart <= #str,
            "argument-error",
            "string index out of bounds")
    end

    return str_new(str:sub(sstart + 1, send))
end

return m
