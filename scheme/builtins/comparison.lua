local m = {}

local util = require "scheme.util"
local types = require "scheme.types"

local expect = util.expect
local expect_argc = util.expect_argc
local expect_argc_min = util.expect_argc_min
local is_true = util.is_true

local bool_new = types.boolean.new

--[[
-- Comparison stuff
--]]
local function is_eq(a, b)
    if a == b then
        return bool_new(true)
    else
        return false
    end
end

local function is_eqv(a, b)
    if is_eq(a, b) then
        return true

    elseif a.type == "number" or a.type == "char" then
        return a:getval() == b:getval()

    elseif a.type == "procedure" then
        if a.builtin and b.builtin then
            return a.body == b.body
        end

    else
        return false
    end
end

local function is_equal(a, b)
    if is_eqv(a, b) then
        return true

    elseif a.type == "string" then
        return a:getval() == b:getval()

    elseif a.type == "list" then
        local a_val, b_val = a:getval(), b:getval()

        if #a_val ~= #b_val then
            return false
        end

        for i = 1, #a_val do
            if not is_equal(a_val[i], b_val[i]) then
                return false
            end
        end

        return true

    else
        return false
    end
end

m["eq?"] = function(self, env, args)
    expect_argc(self, 2, #args)

    return bool_new(is_eq(args[1], args[2]))
end

m["eqv?"] = function(self, env, args)
    expect_argc(self, 2, #args)

    local a, b = args[1], args[2]

    if a.type ~= b.type then
        return bool_new(false)
    else
        return bool_new(is_eqv(a, b))
    end
end

m["equal?"] = function(self, env, args)
    expect_argc(self, 2, #args)

    local a, b = args[1], args[2]

    if a.type ~= b.type then
        return bool_new(false)
    else
        return bool_new(is_equal(a, b))
    end
end


m["not"] = function(self, env, args)
    expect_argc(self, 1, #args)

    return bool_new(not is_true(args[1]))
end

local function boolean_series(op)
    return function(self, env, args)
        local res

        for i, val in ipairs(args) do
            if op(is_true(val)) then
                return val
            end

            res = val
        end

        return res
    end
end

m["and"] = boolean_series(function(b) return not b end)
m["or"]  = boolean_series(function(b) return     b end)

m["xor"] = function(self, env, args)
    expect_argc(self, 2, #args)

    expect(args[1], "boolean", "invalid operand type")
    expect(args[2], args[1].type, "operand type mismatch")

    return bool_new(args[1]:getval() ~= args[2]:getval())
end


local function binary_comparison(typ)
    return function(op)
        return function(self, env, args)
            expect_argc(self, 2, #args)

            local a, b = args[1], args[2]

            expect(a, typ, "invalid operand type")
            expect(b, a.type, "operand type mismatch")

            return bool_new(op(a:getval(), b:getval()))
        end
    end
end

local function series_comparison(typ)
    return function(op)
        return function(self, env, args)
            expect_argc_min(self, 2, #args)

            local res = true

            for i = 2, #args do
                local a, b = args[i - 1], args[i]

                expect(a, typ, "invalid operand type")
                expect(b, a.type, "operand type mismatch")

                if not op(a:getval(), b:getval()) then
                    return bool_new(false)
                end
            end

            return bool_new(true)
        end
    end
end

local numeric_comparison = series_comparison("number")

m["="]  = numeric_comparison(function(a, b) return a == b end)
m["!="] = numeric_comparison(function(a, b) return a ~= b end)
m["<"]  = numeric_comparison(function(a, b) return a <  b end)
m["<="] = numeric_comparison(function(a, b) return a <= b end)
m[">"]  = numeric_comparison(function(a, b) return a >  b end)
m[">="] = numeric_comparison(function(a, b) return a >= b end)

local char_comparison = series_comparison("char")

m["char=?"]  = char_comparison(function(a, b) return a == b end)
m["char!=?"] = char_comparison(function(a, b) return a ~= b end)
m["char<?"]  = char_comparison(function(a, b) return a <  b end)
m["char<=?"] = char_comparison(function(a, b) return a <= b end)
m["char>?"]  = char_comparison(function(a, b) return a >  b end)
m["char>=?"] = char_comparison(function(a, b) return a >= b end)

local string_comparison = binary_comparison("string")

m["string=?"]  = string_comparison(function(a, b) return a == b end)
m["string!=?"] = string_comparison(function(a, b) return a ~= b end)
m["string<?"]  = string_comparison(function(a, b) return a <  b end)
m["string<=?"] = string_comparison(function(a, b) return a <= b end)
m["string>?"]  = string_comparison(function(a, b) return a >  b end)
m["string>=?"] = string_comparison(function(a, b) return a >= b end)

return m
