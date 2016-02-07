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
local string_port_new = types.port.new_string
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

local function make_string_port(self, init, mode)
    return string_port_new(init, mode)
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

m["open-input-string"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "string")

    return make_string_port(self, args[1]:getval(), "r")
end

m["open-output-string"] = function(self, env, args)
    expect_argc(self, 0, #args)

    return make_string_port(self, "", "w")
end

m["close-input-port"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "port")
    ensure(args[1], args[1].mode == "r" and type(args[1]:getval()) ~= "string",
        "type-error",
        "port must be an input port: %s", tostring(args[1]))
    ensure(args[1], args[1]:is_open(),
        "argument-error",
        "port already closed: %s", tostring(args[1]))

    args[1]:close()

    return list_new{}
end

m["close-output-port"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "port")
    ensure(args[1], args[1].mode == "w" and type(args[1]:getval()) ~= "string",
        "type-error",
        "port must be an output port: %s",  tostring(args[1]))
    ensure(args[1], args[1]:is_open(),
        "argument-error",
        "port already closed: %s", tostring(args[1]))

    args[1]:close()

    return list_new{}
end

m["get-output-string"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "port")
    ensure(args[1], args[1].mode == "w" and type(args[1]:getval()) == "string",
        "type-error",
        "port must be an input string port: %s", tostring(args[1]))

    return str_new(args[1]:getval())
end


local function verify_port(port, typ)
    expect(port, "port")
    ensure(port, port.mode == typ,
        "type-error",
        "cannot %s %s-port: %s",
            typ == "w" and "write to" or "read from",
            typ == "w" and "input" or "output",
            tostring(port))

    ensure(port, port:is_open(),
        "argument-error",
        "cannot %s closed port: %s",
            typ == "w" and "write to" or "read from",
            tostring(port))
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

            local f = args[2]:getval()

            if type(f) == "string" then
                -- TODO: Avoid recreating wrapper
                f = setmetatable({str = args[2]}, {
                    __index = {
                        write = function(self, d)
                            self.str.v = self.str.v .. d
                        end
                    }
                })
            end

            fn(f, args[1], args[2])
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

m["write-all"] = iow_function(function(f, arg)
    expect(arg, "string")

    f:write(arg:getval())
end)

m["write-line"] = iow_function(function(f, arg)
    expect(arg, "string")

    f:write(arg:getval() .. "\n")
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

            local f = args[1]:getval()

            if type(f) == "string" then
                -- TODO: Avoid recreating wrapper
                f = setmetatable({str = args[1]}, {
                    __index = {
                        read = function(self, what)
                            local s

                            if what == "*a" then
                                s = self.str.v
                                self.str.v = ""
                            elseif what == "*l" then
                                local i, j
                                i, j, s = self.str.v:find("^(.+)\n")

                                if j then
                                    self.str.v = self.str.v:sub(j + 1)
                                else
                                    s = self.str.v
                                    self.str.v = ""
                                end
                            elseif tonumber(what) ~= nil then
                                s = self.str.v:sub(1, tonumber(what))
                                self.str.v = self.str.v:sub(tonumber(what) + 1)
                            end

                            return s
                        end
                    }
                })
            end

            res = fn(f, args[1])
        else
            res = fn(io.stdin, nil)
        end

        return res and (resfn and resfn(res) or res) or types.port.eof_object
    end
end

m["read"] = ior_function(function(file, port)
    if type(port:getval()) == "string" then
        local p = parser.new_from_string(port:getval())
        local v = p:parse_value()

        port.v = port.v:sub(p.pos)

        return v
    else
        return parser.new_from_open_file(
            file,
            port and port.path or "<stdin>"):parse_value()
    end
end)

m["read-all"]  = ior_function(function(f) return f:read("*a") end, str_new)
m["read-line"] = ior_function(function(f) return f:read("*l") end, str_new)
m["read-char"] = ior_function(function(f) return f:read(1)    end, char_new)

return m
