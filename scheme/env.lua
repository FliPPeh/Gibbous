local env = {}

local parser = require "scheme.parser"
local types = require "scheme.types"
local util = require "scheme.util"

local env_meta

function env.new_environment(lua_env)
    local builtins = require "scheme.builtins.builtins"
    local self = setmetatable({
        env = {
            ["nil"] = types.lua_nil
        },

        name = "root",
        lua_env = lua_env or _G,
        parser = parser.new_from_string(),
        root = true,
        search_paths = package.path:gsub("%.lua", ".scm"),
        imported = {},
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
                    toplevel:preprocess(self)
                end

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
        end,

        intern = function(self, sym)
            sym = sym:lower()
            local s = self.symbols[sym]

            if not s then
                s = types.symbol.new(sym)
                self.symbols[sym] = s
            end

            return s
        end,

        define = function(self, var, val)
            self.env[var:lower()] = val
        end,

        is_defined = function(self, var)
            var = var:lower()

            local special_forms = require "scheme.special_forms"

            if special_forms[var] then
                return true
            end

            return self.env[var] ~= nil
        end,

        resolve = function(self, ident, var)
            if var:sub(1, 1) == ":" then
                -- Lua: t:method(...) -> (:method t ...)
                local meth = var:sub(2)

                return types.proc.wrap_native(var, function(mself, ...)
                    if not mself[meth] then
                        util.err(ident,
                            "not-defined-error",
                            "method %q not defined for Lua type %s",
                            meth,
                            type(mself))
                    end

                    return mself[var:sub(2)](mself, ...)
                end)
            elseif var:sub(1, 1) == "." then
                -- Lua: t.field -> (.field t)
                local field = var:sub(2)

                return types.proc.wrap_native(var, function(mself)
                    return mself[field]
                end)
            end

            if self:is_defined(var) then
                return self.env[var:lower()]
            else
                local function locate_lua(name)
                    local path = util.split_string(name, "%.")
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

                local lv = locate_lua(var)

                if lv then
                    if type(lv) == "function" then
                        local v = types.proc.wrap_native(var, lv)

                        self:define(var, v)
                        return v
                    else
                        return types.toscheme(lv)
                    end
                end

                return nil
            end
        end
    }
}

return env
