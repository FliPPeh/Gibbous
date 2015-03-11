local m = {}

local util = require "scheme.util"
local types = require "scheme.types"

local expect = util.expect
local expect_argc = util.expect_argc

--[[
-- Map stuff
--]]
m["make-map"] = function(self, env, args)
    local map = {}

    for _, v in ipairs(args) do
        expect(v, {"list", "pair"})

        if v.type == "pair" then
            map[types.tolua(v[1])] = types.tolua(v[2])
        else
            map[types.tolua(v[1])] = types.tolua(
                types.list.new{table.unpack(v, 2)})
        end
    end

    return types.map.new(map)
end

m["map-keys"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "map")

    local keys = {}

    for k, _ in pairs(args[1]:getval()) do
        table.insert(keys, types.toscheme(k))
    end

    return types.list.new(keys)
end

m["map-values"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "map")

    local vals = {}

    for _, v in pairs(args[1]:getval()) do
        table.insert(vals, types.toscheme(v))
    end

    return types.list.new(vals)
end

m["map-items"] = function(self, env, args)
    expect_argc(self, 1, #args)
    expect(args[1], "map")

    local res = {}

    for k, v in pairs(args[1]:getval()) do
        table.insert(res, types.pair.new(types.toscheme(k), types.toscheme(v)))
    end

    return types.list.new(res)
end

-- TODO: Find a way to make these work gracefully with table keys?
m["map-get"] = function(self, env, args)
    expect_argc(self, 2, #args)
    expect(args[1], "map")

    return types.toscheme(args[1]:getval()[types.tolua(args[2])])
end

m["map-set!"] = function(self, env, args)
    expect_argc(self, 3, #args)
    expect(args[1], "map")

    args[1]:getval()[types.tolua(args[2])] = types.tolua(args[3])

    return args[1]
end

return m
