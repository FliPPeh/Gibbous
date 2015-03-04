local env = {}

local parser = require "scheme.parser"
local types = require "scheme.types"


local function string_split(str, sep)
    local parts = {}
    local l = 1

    -- While there is a seperator within the string
    while str:find(sep, l) do
        local sep_start, sep_end = str:find(sep, l)

        -- Unless the substring between the last seperators was empty, add
        -- it to the results
        if sep_start ~= l then
            -- Add the part between l (last seperator end or string start) and
            -- sep_start
            table.insert(parts, str:sub(l, sep_start - 1))
        end

        -- put l after the seperator end
        l = sep_end + 1
    end

    if str:len() >= l then
        table.insert(parts, str:sub(l))
    end

    return parts
end

local env_meta

function env.new_environment(lua_env)
    local builtins = require "scheme.builtins"
    local self = setmetatable({
        env = {},
        name = "root",
        lua_env = lua_env or _G,
        parser = parser.new(),
        root = true
    }, env_meta)

    for name, fn in pairs(builtins) do
        self:define(name, types.proc.new_builtin(name, fn))
    end

    return self
end

env_meta = {
    __index = {
        derive = function(self, name)
            return setmetatable({
                env = setmetatable({}, {__index = self.env}),
                name = name,
                lua_env = self.lua_env,
                root = false

            }, env_meta)
        end,

        eval = function(self, chunk)
            local ast = self.parser:parse(chunk)
            return self:eval_ast(ast)
        end,

        eval_file = function(self, file)
            -- Use a dedicated parser each file
            local parser = parser.new(file)

            return self:eval_ast(parser:parse(io.open(file, "r"):read("*a")))
        end,

        eval_ast = function(self, ast)
            local last

            for _, toplevel in ipairs(ast) do
                last = toplevel:eval(self)
            end

            return last
        end,

        define = function(self, var, val)
            -- print(("%s: define %s to: %s"):format(self.name, var, val))
            self.env[var] = val
        end,

        is_defined = function(self, var)
            return self.env[var] ~= nil
        end,

        resolve = function(self, var)
            -- print(("%s: looking up %s"):format(self.name, var))
            if self:is_defined(var) then
                return self.env[var]
            else
                local lv = self:locate_lua(var)

                if lv then
                    if type(lv) == "function" then
                        local function helper(self, args)
                            for i = 1, #args do
                                args[i] = types.tolua(args[i])
                            end

                            local res = {xpcall(function()
                                return lv(table.unpack(args))
                            end, function(err)
                                return types.err.new("lua-error", nil, err)
                            end)}

                            if res[1] then
                                if #res > 2 then
                                    return types.toscheme({
                                        table.unpack(res, 2)
                                    })
                                else
                                    return types.toscheme(res[2])
                                end
                            else
                                error(res[2])
                            end
                        end

                        return types.proc.new_builtin(var, helper)
                    else
                        return types.toscheme(lv)
                    end
                end

                return nil
            end
        end,

        locate_lua = function(self, name)
            local path = string_split(name, "%.")
            local val

            if #path > 1 then
                -- Nested value, lookup
                val = self.lua_env

                for _, e in ipairs(path) do
                    val = val[e]

                    if not val then
                        break
                    end
                end
            else
                -- Not a nested value, access state directly.
                val = self.lua_env[name]
            end

            return val
        end
    }
}

return env
