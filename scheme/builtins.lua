local builtins = {}

local util = require "scheme.util"
local types = require "scheme.types"

--[[
-- Basic stuff
--]]
local function make_port(self, path, mode)
    local f, e = io.open(path, mode)

    if not f then
        util.err(self, "io-error", "failed to open file: %s", e)
    end

    return types.port.new(f, path, mode)

end

builtins["open-input-file"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "string")

    return make_port(self, args[1]:getval(), "r")
end

builtins["open-output-file"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "string")

    return make_port(self, args[1]:getval(), "w")
end


function builtins.display(self, env, args)
    for i, arg in ipairs(args) do
        if arg:type() == "string" or arg:type() == "char" then
            io.write(arg:getval())
        else
            io.write(tostring(arg))
        end
    end

    return types.list.new{}
end

builtins["read-line"] = function(self, env, args)
    util.expect_argc_max(self, 1, #args)

    if #args > 0 then
        util.expect(args[1], "port")

        return types.str.new(args[1]:getval():read("*l"))
    end

    return types.str.new(io.read("*l"))
end

function builtins.format(self, env, args)
    util.expect_argc_min(self, 1, #args)

    local fargs = {}
    for i = 2, #args do
        if args[i]:type() ~= "string" and args[i]:type() ~= "number" then
            table.insert(fargs, tostring(args[i]))
        else
            table.insert(fargs, args[i]:getval())
        end
    end

    return types.str.new(string.format(args[1]:getval(), table.unpack(fargs)))
end

builtins["to-string"] = function(self, env, args)
    util.expect_argc(self, 1, #args)

    return types.str.new(tostring(args[1]))
end

--[[
-- Number stuff
--]]
local function numeric_primitive(op)
    return function(self, env, args)
        util.expect_argc(self, 2, #args)

        local a, b = args[1], args[2]

        util.expect(a, "number", "invalid operand type")
        util.expect(b, a:type(), "operand type mismatch")

        return types.number.new(op(a:getval(), b:getval()))
    end
end

local function unary_numeric_primitive(op)
    return function(self, env, args)
        util.expect_argc(self, 1, #args)
        util.expect(args[1], "number", "invalid operand type")

        return types.number.new(op(args[1]:getval()))
    end
end


builtins["+"] = numeric_primitive(function(a, b) return a + b end)
builtins["-"] = numeric_primitive(function(a, b) return a - b end)
builtins["*"] = numeric_primitive(function(a, b) return a * b end)
builtins["/"] = numeric_primitive(function(a, b) return a / b end)
builtins["%"] = numeric_primitive(function(a, b) return a % b end)
builtins["^"] = numeric_primitive(function(a, b) return a ^ b end)

builtins["neg"] = unary_numeric_primitive(function(a) return -a end)

--[[
-- List stuff
--]]
function builtins.list(self, env, args)
    return types.list.new{table.unpack(args)}
end

function builtins.cons(self, env, args)
    util.expect_argc(self, 2, #args)
    util.expect(args[2], "list")

    return types.list.new{args[1], table.unpack(args[2]:getval())}
end

function builtins.car(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "list")
    util.ensure(args[1], #args[1]:getval() > 0,
        "undefined-error",
        "can not car on empty list")

    return args[1]:getval()[1]
end

function builtins.cdr(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "list")

    return types.list.new{table.unpack(args[1]:getval(), 2)}
end

function builtins.length(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], {"list", "string"})

    return types.number.new(#args[1]:getval())
end

builtins["pair?"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "list")

    return types.boolean.new(#args[1]:getval() == 2)
end

builtins["null?"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "list")

    return types.boolean.new(#args[1]:getval() == 0)
end


--[[
-- Type stuff
--]]
local function is_type(typ)
    return function(self, env, args)
        util.expect_argc(self, 1, #args)

        return types.boolean.new(args[1]:type() == typ)
    end
end

for i, t in ipairs{"symbol",
                   "list",
                   "number",
                   "string",
                   "boolean",
                   "char",
                   "procedure",
                   "port"} do
    builtins[t .. "?"] = is_type(t)
end

function builtins.type(self, env, args)
    util.expect_argc(self, 1, #args)

    return types.str.new(args[1]:type())
end

builtins["symbol->string"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "symbol")

    return types.str.new(args[1]:getval())
end

builtins["string->symbol"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "string")

    return env:intern(args[1]:getval())
end


builtins["list->string"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "list")

    local buf = ""

    for i, c in ipairs(args[1]:getval()) do
        util.expect(c, "char")

        buf = buf .. c:getval()
    end

    return types.str.new(buf)
end

builtins["string->list"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "string")

    local ls = {}

    for i = 1, #args[1]:getval() do
        table.insert(ls, types.char.new(args[1]:getval():sub(i, i)))
    end

    return types.list.new(ls)
end


builtins["number->string"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "number")

    return types.str.new(tonumber(args[1]:getval()))
end

builtins["string->number"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "string")

    return types.number.new(tostring(args[1]:getval()))
end


builtins["char->integer"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "char")

    return types.number.new(args[1]:getval():byte(1))
end

builtins["integer->char"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "number")

    return types.char.new(string.char(args[1]:getval()))
end


--[[
-- Comparison stuff
--]]
builtins["not"] = function(self, env, args)
    util.expect_argc(self, 1, #args)
    util.expect(args[1], "boolean")

    return types.boolean.new(not args[1]:getval())
end

local function boolean_operator(op)
    return function(self, env, args)
        util.expect_argc(self, 2, #args)

        util.expect(args[1], "boolean", "invalid operand type")
        util.expect(args[2], args[1]:type(), "operand type mistmatch")

        return types.boolean.new(op(args[1]:getval(), args[2]:getval()))
    end
end

builtins["and"] = boolean_operator(function(a, b) return a and b end)
builtins["or"]  = boolean_operator(function(a, b) return a  or b end)
builtins["xor"] = boolean_operator(function(a, b) return a ~=  b end)

builtins["="] = function(self, env, args)
    util.expect_argc(self, 2, #args)

    local a, b = args[1], args[2]

    util.expect(a, {"symbol",
                    "number",
                    "string",
                    "bool",
                    "list",
                    "char"}, "invalid operand type")

    -- No type coercion
    util.expect(b, a:type(), "operand type mismatch")

    if a:type() == "symbol" then
        return types.boolean.new(a == b)
    elseif a:type() == "number" or
           a:type() == "string" or
           a:type() == "char" or
           a:type() == "bool" then
        return types.boolean.new(a:getval() == b:getval())
    else
        local av, bv = a:getval(), b:getval()

        if #av ~= #bv then
            return types.boolean.new(false)
        end

        for i = 1, #av do
            if not builtins["="](self, env, {av[i], bv[i]}):getval() then
                return types.boolean.new(false)
            end
        end

        return types.boolean.new(true)
    end
end

builtins["!="] = function(self, env, args)
    return builtins["not"](self, env, {builtins["="](self, env, args)})
end

builtins["<"] = function(self, env, args)
    util.expect_argc(self, 2, #args)

    local a, b = args[1], args[2]

    util.expect(a, {"number", "string"}, "invalid operand type")
    util.expect(b, a:type(), "operand type mismatch")

    return types.boolean.new(a:getval() < b:getval())
end

builtins["<="] = function(self, env, args)
    util.expect_argc(self, 2, #args)

    return builtins["not"](self, env,
        {builtins["<"](self, env, {args[2], args[1]})})
end

builtins[">"] = function(self, env, args)
    util.expect_argc(self, 2, #args)

    return builtins["<"](self, env, {args[2], args[1]})
end

builtins[">="] = function(self, env, args)
    return builtins["not"](self,  env, {builtins["<"](self, env, args)})
end

return builtins
