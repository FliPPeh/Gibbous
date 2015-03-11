local builtins = {}

local util = require "scheme.util"
local types = require "scheme.types"
local parser = require "scheme.parser"

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

builtins["close-input-port"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "port")
    ensure(args[1], args[1].mode == "r",
        "type-error",
        "port must be an input port: %s", args[1])
    ensure(args[1], args[1]:is_open(),
        "argument-error",
        "port already closed: %s", args[1])

    args[1]:close()

    return list_new{}
end

builtins["close-output-port"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "port")
    ensure(args[1], args[1].mode == "w",
        "type-error",
        "port must be an output port: %s", args[1])
    ensure(args[1], args[1]:is_open(),
        "argument-error",
        "port already closed: %s", args[1])

    args[1]:close()

    return list_new{}
end


local function repr_display(arg)
    if arg.type == "symbol" or
       arg.type == "string" or
       arg.type == "number" or
       arg.type == "char" then

       return arg:getval()

    elseif arg.type == "list" then
        local parts = {}

        for i, v in ipairs(arg:getval()) do
            table.insert(parts, repr_display(v))
        end

        return "(" .. table.concat(parts, " ") .. ")"
    elseif arg.type == "pair" then
        local v2s = repr_display(arg:getval()[2])

        if arg:getval()[2].type == "pair" then
            v2s = v2s:sub(2, #v2s - 1)
            return ("(%s %s)"):format(repr_display(arg:getval()[1]), v2s)
        else
            return ("(%s . %s)"):format(repr_display(arg:getval()[1]), v2s)
        end
    else
        return tostring(arg)
    end
end

local function verify_port(port, typ)
    expect(port, "port")
    ensure(port, port.mode == typ,
        "type-error",
        "cannot write to %s-port: %s", typ == "w" and "input" or "output", port)

    ensure(port, port:is_open(),
        "argument-error",
        "cannot write to closed port", port)
end

builtins["newline"] = function(self, env, args)
    expect_argc_max(self, 1, #args)

    if #args == 0 then
        io.write("\n")
    else
        verify_port(args[1], "w")
        args[1]:getval():write("\n")
    end

    return list_new{}
end

builtins["display"] = function(self, env, args)
    expect_argc_min(self, 1, #args)
    expect_argc_max(self, 2, #args)

    if #args == 1 then
        io.write(repr_display(args[1]))
    else
        verify_port(args[2], "w")

        args[2]:getval():write(repr_display(args[1]))
    end

    return list_new{}
end

builtins["write"] = function(self, env, args)
    expect_argc_min(self, 1, #args)
    expect_argc_max(self, 2, #args)

    local str

    if args[1].type == "list" or
       args[1].type == "pair" or
       args[1].type == "symbol" then
        str = "'" .. tostring(args[1])
    else
        str = tostring(args[1])
    end


    if #args == 1 then
        io.write(str)
    else
        verify_port(args[2], "w")

        args[2]:getval():write(str)
    end

    return list_new{}
end

builtins["write-char"] = function(self, env, args)
    expect_argc_min(self, 1, #args)
    expect_argc_max(self, 2, #args)

    expect(args[1], "char")

    if #args == 1 then
        io.write(args[1]:getval())
    else
        verify_port(args[2], "w")

        args[2]:getval():write(args[1]:getval())
    end

    return list_new{}
end

builtins["read"] = function(self, env, args)
    expect_argc_max(self, 1, #args)

    local obj

    if #args > 0 then
        verify_port(args[1], "r")

        obj = parser.new_from_open_file(
            args[1]:getval(),
            args[1].path):parse_value()
    else
        obj = parser.new_from_open_file(io.stdin, "<stdin>"):parse_value()
    end

    return obj or types.port.eof_object
end

builtins["read-line"] = function(self, env, args)
    expect_argc_max(self, 1, #args)

    local line

    if #args > 0 then
        verify_port(args[1], "r")

        line = args[1]:getval():read("*l")
    else
        line = io.read("*l")
    end

    return line and str_new(line) or types.port.eof_object
end

builtins["read-char"] = function(self, env, args)
    expect_argc_max(self, 1, #args)

    local c

    if #args > 0 then
        verify_port(args[1], "r")

        c = args[1]:getval():read(1)
    else
        c = io.read(1)
    end

    return c and char_new(c) or types.port.eof_object
end

builtins["format"] = function(self, env, args)
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
        expect(b, a.type, "operand type mismatch")

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
builtins["cons"] = function(self, env, args)
    expect_argc(self, 2, #args)

    if args[2].type == "list" then
        return list_new{args[1], table.unpack(args[2]:getval())}
    else
        return pair_new(args[1], args[2])
    end
end

builtins["car"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], {"pair", "list"})
    ensure(args[1], #args[1]:getval() > 0,
        "undefined-error",
        "can not car on empty list")

    return args[1]:getval()[1]
end

builtins["cdr"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], {"pair", "list"})

    if args[1].type == "list" then
        return list_new{table.unpack(args[1]:getval(), 2)}
    else
        return args[1]:getval()[2]
    end
end

builtins["length"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "list")

    return number_new(#args[1]:getval())
end

builtins["null?"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "list")

    return bool_new(#args[1]:getval() == 0)
end

--[[
-- String stuff
--]]
builtins["string-length"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return number_new(#args[1]:getval())
end

builtins["string-null?"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return bool_new(#args[1]:getval() == 0)
end

builtins["string-append"] = function(self, env, args)
    local res = ""

    for _, str in ipairs(args) do
        expect(str, "string")

        res = res .. str:getval()
    end

    return str_new(res)
end

builtins["string-join"] = function(self, env, args)
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

builtins["string-split"] = function(self, env, args)
    expect_argc_min(self, 1, #args)
    expect(args[1], "string")

    local sep = "%s"

    if #args == 2 then
        expect(args[2], "string")

        sep = util.literal_pattern(args[2]:getval())
    end

    return types.toscheme(util.split_string(args[1]:getval(), sep))
end

builtins["string-trim"] = function(self, env, args)
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


builtins["string-ref"] = function(self, env, args)
    expect_argc(self, 2, #args)
    expect(args[1], "string")
    expect(args[2], "number")
    ensure(args[2], (args[2]:getval() + 1) <= #args[1]:getval(),
        "argument-error",
        "string index out of bounds")

    local ind = args[2]:getval() + 1

    return char_new(args[1]:getval():sub(ind, ind))
end

builtins["substring"] = function(self, env, args)
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

--[[
-- Type stuff
--]]
local function is_type(typ)
    return function(self, env, args)
        expect_argc(self, 1, #args)

        return bool_new(args[1].type == typ)
    end
end

for i, t in ipairs{"symbol", "pair", "list", "number", "string", "boolean",
                   "char", "procedure", "port", "eof-object", "error"} do
    builtins[t .. "?"] = is_type(t)
end

-- Special functions for input and output ports
builtins["input-port?"] = function(self, env, args)
    expect_argc(self, 1, #args)

    return bool_new(args[1].type == "port" and args[1].mode == "r")
end

builtins["output-port?"] = function(self, env, args)
    expect_argc(self, 1, #args)

    return bool_new(args[1].type == "port" and args[1].mode == "w")
end


builtins["type"] = function(self, env, args)
    expect_argc(self, 1, #args)

    return str_new(args[1].type)
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

builtins["eq?"] = function(self, env, args)
    expect_argc(self, 2, #args)

    return bool_new(is_eq(args[1], args[2]))
end

builtins["eqv?"] = function(self, env, args)
    expect_argc(self, 2, #args)

    local a, b = args[1], args[2]

    if a.type ~= b.type then
        return bool_new(false)
    else
        return bool_new(is_eqv(a, b))
    end
end

builtins["equal?"] = function(self, env, args)
    expect_argc(self, 2, #args)

    local a, b = args[1], args[2]

    if a.type ~= b.type then
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

builtins["="]  = numeric_comparison(function(a, b) return a == b end)
builtins["!="] = numeric_comparison(function(a, b) return a ~= b end)
builtins["<"]  = numeric_comparison(function(a, b) return a <  b end)
builtins["<="] = numeric_comparison(function(a, b) return a <= b end)
builtins[">"]  = numeric_comparison(function(a, b) return a >  b end)
builtins[">="] = numeric_comparison(function(a, b) return a >= b end)

local char_comparison = series_comparison("char")

builtins["char=?"]  = char_comparison(function(a, b) return a == b end)
builtins["char!=?"] = char_comparison(function(a, b) return a ~= b end)
builtins["char<?"]  = char_comparison(function(a, b) return a <  b end)
builtins["char<=?"] = char_comparison(function(a, b) return a <= b end)
builtins["char>?"]  = char_comparison(function(a, b) return a >  b end)
builtins["char>=?"] = char_comparison(function(a, b) return a >= b end)

local string_comparison = binary_comparison("string")

builtins["string=?"]  = string_comparison(function(a, b) return a == b end)
builtins["string!=?"] = string_comparison(function(a, b) return a ~= b end)
builtins["string<?"]  = string_comparison(function(a, b) return a <  b end)
builtins["string<=?"] = string_comparison(function(a, b) return a <= b end)
builtins["string>?"]  = string_comparison(function(a, b) return a >  b end)
builtins["string>=?"] = string_comparison(function(a, b) return a >= b end)

return builtins
