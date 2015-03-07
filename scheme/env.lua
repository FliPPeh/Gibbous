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

local function make_lua_wrapper(name, wrapped_function)
    return function(self, env, args)
        for i = 1, #args do
            args[i] = types.tolua(args[i])
        end

        local res = {xpcall(function()
            return wrapped_function(table.unpack(args))

        end, function(err)
            local f, l, c = self:getpos()
            local location = {
                file = f,
                line = l,
                col  = c
            }

            return types.err.new("lua-error", location,
                ("in Lua function %s: %s"):format(name, err))
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
end


local env_meta

function env.new_environment(lua_env)
    local builtins = require "scheme.builtins"
    local self = setmetatable({
        env = {},
        name = "root",
        lua_env = lua_env or _G,
        parser = parser.new_from_string(),
        root = true,
        symbols = {},
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
                root = false,

            }, { __index = self})
        end,

        eval = function(self, chunk)
            self.parser:feed(chunk)

            local ast = self.parser:parse()
            return self:eval_ast(ast)
        end,

        eval_file = function(self, file)
            -- Use a dedicated parser each file
            local parser = parser.new_from_file(file)

            return self:eval_ast(parser:parse())
        end,

        eval_ast = function(self, ast)
            local ok, res = xpcall(function()
                local last

                for _, toplevel in ipairs(ast) do
                    last = toplevel:eval(self)
                end

                return last
            end, function(err)
                if type(err) ~= "table" then
                    return debug.traceback(err, 1)
                else
                    return err
                end
            end)

            if ok then
                return res
            else
                print("Uncaught error:")

                if type(res) == "table" then
                    print(res:tostring())
                else
                    print(res)
                end
            end
            --]]
        end,

        intern = function(self, sym)
            local s = self.symbols[sym:lower()]

            if not s then
                s = types.symbol.new(sym:lower())
                self.symbols[sym:lower()] = s
            end

            return s
        end,

        define = function(self, var, val)
            -- print(("%s: define %s to: %s"):format(self.name, var, val))
            self.env[var:lower()] = val
        end,

        is_defined = function(self, var)
            local special_forms = require "scheme.special_forms"

            if special_forms[var:lower()] then
                return true
            end

            return self.env[var:lower()] ~= nil
        end,

        resolve = function(self, var)
            -- print(("%s: looking up %s"):format(self.name, var))
            if self:is_defined(var) then
                return self.env[var:lower()]
            else
                local lv = self:locate_lua(var)

                if lv then
                    if type(lv) == "function" then
                        local v = types.proc.new_builtin(var,
                            make_lua_wrapper(var, lv))

                        self:define(var, v)
                        return v
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
