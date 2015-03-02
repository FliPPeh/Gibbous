local env = {}

local parser = require "scheme.parser"


local env_meta

function env.new_environment(lua_env)
    return setmetatable({
        env = {},
        name = "root",
        lua_env = lua_env or _G,
        parser = parser.new(),
        root = true
    }, env_meta)
end

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
            return self.env[var]
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
