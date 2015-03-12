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


local function verify_port(port, typ)
    expect(port, "port")
    ensure(port, port.mode == typ,
        "type-error",
        "cannot write to %s-port: %s", typ == "w" and "input" or "output", port)

    ensure(port, port:is_open(),
        "argument-error",
        "cannot write to closed port", port)
end

--[[
-- Writing
--]]
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

local function iow_function(fn)
    return function(self, env, args)
        expect_argc_min(self, 1, #args)
        expect_argc_max(self, 2, #args)

        if #args == 1 then
            fn(io.stdout, args[1], nil)
        else
            verify_port(args[2], "w")

            fn(args[2]:getval(), args[2], args[1])
        end

        return list_new{}
    end
end

m["display"] = iow_function(function(f, arg)
    f:write(util.display_repr(arg))
end)

m["write"] = iow_function(function(f, arg)
    f:write(util.write_repr(arg))
end)

m["write-char"] = iow_function(function(f, arg)
    expect(arg, "char")

    f:write(arg:getval())
end)

--[[
-- Reading
--]]
local function ior_function(fn, resfn)
    return function(self, env, args)
        expect_argc_max(self, 1, #args)

        local res

        if #args > 0 then
            verify_port(args[1], "r")

            res = fn(args[1]:getval(), args[1])
        else
            res = fn(io.stdin, nil)
        end

        return res and (resfn and resfn(res) or res) or types.port.eof_object
    end
end

m["read"] = ior_function("r", function(file, port)
    return parser.new_from_open_file(
        file,
        port and port.path or "<stdin>"):parse_value()
end)

m["read-all"]  = ior_function(function(f) return f:read("*a") end, str_new)
m["read-line"] = ior_function(function(f) return f:read("*l") end, str_new)
m["read-char"] = ior_function(function(f) return f:read(1)    end, char_new)

return m
