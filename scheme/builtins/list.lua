local m = {}

local util = require "scheme.util"
local types = require "scheme.types"

local ensure = util.ensure
local expect = util.expect
local expect_argc = util.expect_argc

local pair_new = types.pair.new
local list_new = types.list.new
local number_new = types.number.new
local bool_new = types.boolean.new

--[[
-- List stuff
--]]
m["cons"] = function(self, env, args)
    expect_argc(self, 2, #args)

    if args[2].type == "list" then
        return list_new{args[1], table.unpack(args[2]:getval())}
    else
        return pair_new(args[1], args[2])
    end
end

m["car"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], {"pair", "list"})
    ensure(args[1], #args[1]:getval() > 0,
        "undefined-error",
        "can not car on empty list")

    return args[1]:getval()[1]
end

m["cdr"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], {"pair", "list"})

    if args[1].type == "list" then
        return list_new{table.unpack(args[1]:getval(), 2)}
    else
        return args[1]:getval()[2]
    end
end

m["length"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "list")

    return number_new(#args[1]:getval())
end

m["null?"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "list")

    return bool_new(#args[1]:getval() == 0)
end

return m
