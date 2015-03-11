local m = {}

local util = require "scheme.util"
local types = require "scheme.types"

local expect = util.expect
local expect_argc = util.expect_argc

local number_new = types.number.new
local char_new = types.char.new
local str_new = types.str.new
local list_new = types.list.new
local bool_new = types.boolean.new

--[[
-- Type stuff
--]]
local function is_type(typ)
    return function(self, env, args)
        expect_argc(self, 1, #args)

        return bool_new(args[1].type == typ)
    end
end

for i, t in ipairs{
        "symbol",
        "pair",
        "list",
        "number",
        "string",
        "boolean",
        "char",
        "procedure",
        "port",
        "eof-object",
        "error"} do
    m[t .. "?"] = is_type(t)
end

-- Special functions for input and output ports
m["input-port?"] = function(self, env, args)
    expect_argc(self, 1, #args)

    return bool_new(args[1].type == "port" and args[1].mode == "r")
end

m["output-port?"] = function(self, env, args)
    expect_argc(self, 1, #args)

    return bool_new(args[1].type == "port" and args[1].mode == "w")
end


m["type"] = function(self, env, args)
    expect_argc(self, 1, #args)

    return str_new(args[1].type)
end

m["symbol->string"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "symbol")

    return str_new(args[1]:getval())
end

m["string->symbol"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return env:intern(args[1]:getval())
end


m["list->string"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "list")

    local buf = ""

    for i, c in ipairs(args[1]:getval()) do
        expect(c, "char")

        buf = buf .. c:getval()
    end

    return str_new(buf)
end

m["string->list"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    local ls = {}

    for i = 1, #args[1]:getval() do
        table.insert(ls, char_new(args[1]:getval():sub(i, i)))
    end

    return list_new(ls)
end


m["number->string"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "number")

    return str_new(tonumber(args[1]:getval()))
end

m["string->number"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return number_new(tostring(args[1]:getval()))
end


m["char->integer"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "char")

    return number_new(args[1]:getval():byte(1))
end

m["integer->char"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "number")

    return char_new(string.char(args[1]:getval()))
end

return m
