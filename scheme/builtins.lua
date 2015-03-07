local builtins = {}

local util = require "scheme.util"
local types = require "scheme.types"

local err = util.err
local ensure = util.ensure
local expect = util.expect
local expect_argc = util.expect_argc
local expect_argc_min = util.expect_argc_min
local expect_argc_max = util.expect_argc_max
local is_true = util.is_true

local port_new = types.port.new
local number_new = types.number.new
local char_new = types.char.new
local str_new = types.str.new
local pair_new = types.pair.new
local list_new = types.list.new
local bool_new = types.boolean.new

--[[
-- Basic stuff
--]]
local function make_port(self, path, mode)
    local f, e = io.open(path, mode)

    if not f then
        err(self, "io-error", "failed to open file: %s", e)
    end

    return port_new(f, path, mode)

end

builtins["open-input-file"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return make_port(self, args[1]:getval(), "r")
end

builtins["open-output-file"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return make_port(self, args[1]:getval(), "w")
end


local function repr_display(arg)
    if arg:type() == "symbol" or
       arg:type() == "string" or
       arg:type() == "number" or
       arg:type() == "char" then

       return arg:getval()

    elseif arg:type() == "list" then
        local parts = {}

        for i, v in ipairs(arg:getval()) do
            table.insert(parts, repr_display(v))
        end

        return "(" .. table.concat(parts, " ") .. ")"
    elseif arg:type() == "pair" then
        local v2s = repr_display(arg:getval()[2])

        if arg:getval()[2]:type() == "pair" then
            v2s = v2s:sub(2, #v2s - 1)
            return ("(%s %s)"):format(repr_display(arg:getval()[1]), v2s)
        else
            return ("(%s . %s)"):format(repr_display(arg:getval()[1]), v2s)
        end
    else
        return tostring(arg)
    end
end


function builtins.display(self, env, args)
    expect_argc(self, 1, #args)

    io.write(repr_display(args[1]))

    return list_new{}
end

function builtins.write(self, env, args)
    expect_argc(self, 1, #args)

    if args[1]:type() == "list" or
       args[1]:type() == "pair" or
       args[1]:type() == "symbol" then
        io.write("'" .. tostring(args[1]))
    else
        io.write(tostring(args[1]))
    end

    return list_new{}
end


builtins["read-line"] = function(self, env, args)
    expect_argc_max(self, 1, #args)

    if #args > 0 then
        expect(args[1], "port")

        return str_new(args[1]:getval():read("*l"))
    end

    return str_new(io.read("*l"))
end

function builtins.format(self, env, args)
    expect_argc_min(self, 1, #args)

    local fargs = {}
    for i = 2, #args do
        if args[i]:type() ~= "string" and args[i]:type() ~= "number" then
            table.insert(fargs, tostring(args[i]))
        else
            table.insert(fargs, args[i]:getval())
        end
    end

    return str_new(string.format(args[1]:getval(), table.unpack(fargs)))
end

builtins["to-string"] = function(self, env, args)
    expect_argc(self, 1, #args)

    return str_new(tostring(args[1]))
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
    expect_argc(self, 2, #args)
    expect(args[1], "procedure")
    expect(args[2], "list")

    return args[1]:call(env, args[2]:getval())
end

builtins["raise"] = function(self, env, args)
    expect_argc(self, 1, #args)

    print(self)
    util.err(self, "user-error", args[1])
end

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


builtins["+"] = numeric_primitive(function(a, b) return a + b end, 0)
builtins["*"] = numeric_primitive(function(a, b) return a * b end, 1)
builtins["-"] = unary_numeric_primitive(function(a, b) return a - b end, 0)
builtins["/"] = unary_numeric_primitive(function(a, b) return a / b end, 1)

local function binary_numeric_primitive(op)
    return function(self, env, args)
        expect_argc(self, 2, #args)

        local a, b = args[1], args[2]

        expect(a, "number", "invalid operand type")
        expect(b, a:type(), "operand type mismatch")

        return number_new(op(a:getval(), b:getval()))
    end
end

-- Remainder could be implemented with interop, but it is related to modulo
-- which can't be implemented with interop, so we'll group it here.
builtins["remainder"] = binary_numeric_primitive(function(a, b)
    return math.fmod(a, b)
end)

builtins["modulo"] = binary_numeric_primitive(function(a, b)
    return a % b
end)


--[[
-- List stuff
--]]
function builtins.cons(self, env, args)
    expect_argc(self, 2, #args)

    if args[2]:type() == "list" then
        return list_new{args[1], table.unpack(args[2]:getval())}
    else
        return pair_new(args[1], args[2])
    end
end

function builtins.car(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], {"pair", "list"})
    ensure(args[1], #args[1]:getval() > 0,
        "undefined-error",
        "can not car on empty list")

    return args[1]:getval()[1]
end

function builtins.cdr(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], {"pair", "list"})

    if args[1]:type() == "list" then
        return list_new{table.unpack(args[1]:getval(), 2)}
    else
        return args[1]:getval()[2]
    end
end

function builtins.length(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], {"list", "string"})

    return number_new(#args[1]:getval())
end

builtins["pair?"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "list")

    return bool_new(#args[1]:getval() == 2)
end

builtins["null?"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "list")

    return bool_new(#args[1]:getval() == 0)
end


--[[
-- Type stuff
--]]
local function is_type(typ)
    return function(self, env, args)
        expect_argc(self, 1, #args)

        return bool_new(args[1]:type() == typ)
    end
end

for i, t in ipairs{"symbol",
                   "pair",
                   "list",
                   "number",
                   "string",
                   "boolean",
                   "char",
                   "procedure",
                   "port",
                   "error"} do
    builtins[t .. "?"] = is_type(t)
end

function builtins.type(self, env, args)
    expect_argc(self, 1, #args)

    return str_new(args[1]:type())
end

builtins["symbol->string"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "symbol")

    return str_new(args[1]:getval())
end

builtins["string->symbol"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return env:intern(args[1]:getval())
end


builtins["list->string"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "list")

    local buf = ""

    for i, c in ipairs(args[1]:getval()) do
        expect(c, "char")

        buf = buf .. c:getval()
    end

    return str_new(buf)
end

builtins["string->list"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    local ls = {}

    for i = 1, #args[1]:getval() do
        table.insert(ls, char_new(args[1]:getval():sub(i, i)))
    end

    return list_new(ls)
end


builtins["number->string"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "number")

    return str_new(tonumber(args[1]:getval()))
end

builtins["string->number"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return number_new(tostring(args[1]:getval()))
end


builtins["char->integer"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "char")

    return number_new(args[1]:getval():byte(1))
end

builtins["integer->char"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "number")

    return char_new(string.char(args[1]:getval()))
end


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

    elseif a:type() == "number" or a:type() == "char" then
        return a:getval() == b:getval()

    elseif a:type() == "procedure" then
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

    elseif a:type() == "string" then
        return a:getval() == b:getval()

    elseif a:type() == "list" then
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

builtins["eq?"] = function(self, env, args)
    expect_argc(self, 2, #args)

    return bool_new(is_eq(args[1], args[2]))
end

builtins["eqv?"] = function(self, env, args)
    expect_argc(self, 2, #args)

    local a, b = args[1], args[2]

    if a:type() ~= b:type() then
        return bool_new(false)
    else
        return bool_new(is_eqv(a, b))
    end
end

builtins["equal?"] = function(self, env, args)
    expect_argc(self, 2, #args)

    local a, b = args[1], args[2]

    if a:type() ~= b:type() then
        return bool_new(false)
    else
        return bool_new(is_equal(a, b))
    end
end


builtins["not"] = function(self, env, args)
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

builtins["and"] = boolean_series(function(b) return not b end)
builtins["or"]  = boolean_series(function(b) return     b end)

builtins["xor"] = function(self, env, args)
    expect_argc(self, 2, #args)

    expect(args[1], "boolean", "invalid operand type")
    expect(args[2], args[1]:type(), "operand type mismatch")

    return bool_new(args[1]:getval() ~= args[2]:getval())
end


local function binary_comparison(typ)
    return function(op)
        return function(self, env, args)
            expect_argc(self, 2, #args)

            local a, b = args[1], args[2]

            expect(a, typ, "invalid operand type")
            expect(b, a:type(), "operand type mismatch")

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
                expect(b, a:type(), "operand type mismatch")

                if not op(a:getval(), b:getval()) then
                    return bool_new(false)
                end
            end

            return bool_new(true)
        end
    end
end

local numeric_comparison = series_comparison("number")

builtins["="]  = numeric_comparison(function(a, b) return a == b end)
builtins["!="] = numeric_comparison(function(a, b) return a ~= b end)
builtins["<"]  = numeric_comparison(function(a, b) return a <  b end)
builtins["<="] = numeric_comparison(function(a, b) return a <= b end)
builtins[">"]  = numeric_comparison(function(a, b) return a >  b end)
builtins[">="] = numeric_comparison(function(a, b) return a >= b end)

local string_comparison = binary_comparison("string")

builtins["string="]  = string_comparison(function(a, b) return a == b end)
builtins["string!="] = string_comparison(function(a, b) return a ~= b end)
builtins["string<"]  = string_comparison(function(a, b) return a <  b end)
builtins["string<="] = string_comparison(function(a, b) return a <= b end)
builtins["string>"]  = string_comparison(function(a, b) return a >  b end)
builtins["string>="] = string_comparison(function(a, b) return a >= b end)

return builtins
