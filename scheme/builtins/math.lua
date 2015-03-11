local m = {}

local util = require "scheme.util"
local types = require "scheme.types"

local expect = util.expect
local expect_argc = util.expect_argc
local expect_argc_min = util.expect_argc_min

local number_new = types.number.new

--[[
-- Number stuff
--]]
local function numeric_primitive(op, ident)
    return function(self, env, args)
        expect_argc_min(self, 0, #args)

        local res = ident

        for i = 1, #args do
            local a = args[i]

            expect(a, "number", "invalid operand type")

            res = op(res, a:getval())
        end

        return number_new(res)
    end
end

local function unary_numeric_primitive(op, ident)
    return function(self, env, args)
        expect_argc_min(self, 1, #args)

        expect(args[1], "number", "invalid operand type")

        if #args == 1 then
            return number_new(op(ident, args[1]:getval()))
        else
            local res = args[1]:getval()

            for i = 2, #args do
                local a = args[i]

                expect(a, "number", "invalid operand type")

                res = op(res, a:getval())
            end

            return number_new(res)
        end
    end
end


m["+"] = numeric_primitive(function(a, b) return a + b end, 0)
m["*"] = numeric_primitive(function(a, b) return a * b end, 1)
m["-"] = unary_numeric_primitive(function(a, b) return a - b end, 0)
m["/"] = unary_numeric_primitive(function(a, b) return a / b end, 1)

local function binary_numeric_primitive(op)
    return function(self, env, args)
        expect_argc(self, 2, #args)

        local a, b = args[1], args[2]

        expect(a, "number", "invalid operand type")
        expect(b, a.type, "operand type mismatch")

        return number_new(op(a:getval(), b:getval()))
    end
end

-- Remainder could be implemented with interop, but it is related to modulo
-- which can't be implemented with interop, so we'll group it here.
m["remainder"] = binary_numeric_primitive(function(a, b)
    return math.fmod(a, b)
end)

m["modulo"] = binary_numeric_primitive(function(a, b)
    return a % b
end)

return m
