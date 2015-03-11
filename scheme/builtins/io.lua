local m = {}

local util = require "scheme.util"
local types = require "scheme.types"
local parser = require "scheme.parser"

local ensure = util.ensure
local expect = util.expect
local expect_argc = util.expect_argc
local expect_argc_min = util.expect_argc_min
local expect_argc_max = util.expect_argc_max

local port_new = types.port.new
local char_new = types.char.new
local str_new = types.str.new
local list_new = types.list.new

--[[
-- I/O stuff
--]]
local function make_port(self, path, mode)
    local f, e = io.open(path, mode)

    if not f then
        err(self, "io-error", "failed to open file: %s", e)
    end

    return port_new(f, path, mode)

end

m["open-input-file"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return make_port(self, args[1]:getval(), "r")
end

m["open-output-file"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return make_port(self, args[1]:getval(), "w")
end

m["close-input-port"] = function(self, env, args)
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

m["close-output-port"] = function(self, env, args)
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

m["newline"] = function(self, env, args)
    expect_argc_max(self, 1, #args)

    if #args == 0 then
        io.write("\n")
    else
        verify_port(args[1], "w")
        args[1]:getval():write("\n")
    end

    return list_new{}
end

m["display"] = function(self, env, args)
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

m["write"] = function(self, env, args)
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

m["write-char"] = function(self, env, args)
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

m["read"] = function(self, env, args)
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

m["read-line"] = function(self, env, args)
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

m["read-char"] = function(self, env, args)
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

return m
