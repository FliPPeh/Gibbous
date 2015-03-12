local types = {}

local util = require "scheme.util"
local special_forms = require "scheme.special_forms"

local err = util.err
local ensure = util.ensure
local expect = util.expect
local expect_argc = util.expect_argc
local expect_argc_min = util.expect_argc_min
local expect_argc_max = util.expect_argc_max


local ascii_names = {
    "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
    "BS",  "TAB", "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US"
}

local charnames = {
    DEL     = string.char(127)
}

for i, name in ipairs(ascii_names) do
    charnames[name] = string.char(i - 1)
end

charnames.altmode   = charnames.ESC
charnames.backnext  = charnames.US
charnames.backspace = charnames.BS
charnames.call      = charnames.SUB
charnames.linefeed  = charnames.LF
charnames.newline   = charnames.LF
charnames.page      = charnames.FF
charnames["return"] = charnames.CR
charnames.rubout    = charnames.DEL
charnames.space     = " "
charnames.tab       = charnames.TAB

types.charnames = charnames

types.base_meta = {
    __index = {
        setpos = function(self, file, line, col)
            self.def_file = file
            self.def_line = line
            self.def_col  = col
        end,

        setevalpos = function(self, file, line, col)
            self.eval_file = file
            self.eval_line = line
            self.eval_col  = col
        end,

        getdefpos = function(self)
            return self.def_file, self.def_line, self.def_col
        end,

        getpos = function(self)
            return self.eval_file or self.def_file,
                   self.eval_line or self.def_line,
                   self.eval_col  or self.def_col
        end,

        getval = function(self)
            return self.v
        end,

        eval = function(self, env)
            err(self,
                "evaluation-error",
                "cannot evaluate value of type %s: %s",
                    self.type,
                    self)
        end,
    }
}

--[[
-- Primitive types
--]]
types.symbol = {
    new = function(sym)
        return setmetatable({
            v = sym
        }, types.symbol_meta)
    end
}

types.symbol_meta = {
    __tostring = function(self)
        return self.v
    end,

    __index = setmetatable({
        type = "symbol",

        eval = function(self, env)
            return env:intern(self.v)
        end

    }, types.base_meta)
}


types.ident = {
    new = function(ident)
        return setmetatable({
            v = ident
        }, types.ident_meta)
    end
}

types.ident_meta = {
    __tostring = function(self)
        return self.v
    end,

    __index = setmetatable({
        type = "identifier",

        eval = function(self, env)
            local val = self

            while val.type == "identifier" do
                nval = env:resolve(val:getval())

                if not nval then
                    err(self,
                        "not-defined-error",
                        "unresolved procedure or variable: %s",
                            val:getval())
                end

                val = nval
            end

            return val
        end

    }, types.base_meta)
}


types.str = {
    new = function(str)
        return setmetatable({
            v = str
        }, types.str_meta)
    end
}

types.str_meta = {
    __tostring = function(self)
        return string.format("%q", self.v)
    end,

    __index = setmetatable({
        type = "string",

        eval = function(self, env)
            return self
        end
    }, types.base_meta)
}


types.number = {
    new = function(num)
        return setmetatable({
            v = num
        }, types.number_meta)
    end
}

types.number_meta = {
    __tostring = function(self)
        return string.format("%s", self.v)
    end,

    __index = setmetatable({
        type = "number",

        eval = function(self, env)
            return self
        end
    }, types.base_meta)
}


types.char = {
    new = function(char)
        return setmetatable({
            v = char
        }, types.char_meta)
    end
}

types.char_meta = {
    __tostring = function(self)
        local w = self.v

        if not self.v:find("[^%c%s]") then
            -- prepare hex representation in case we can't locate a name for it
            w = ("x%x"):format(string.byte(w, 1))

            for k, v in pairs(charnames) do
                if self.v == v then
                    w = k
                    break
                end
            end
        end

        return "#\\" .. w
    end,

    __index = setmetatable({
        type = "char",

        eval = function(self, env)
            return self
        end
    }, types.base_meta)
}


types.boolean = {
    new = function(bool)
        if bool then
            if not types.boolean.true_val then
                types.boolean.true_val =
                    setmetatable({v = true},  types.boolean_meta)
            end

            return types.boolean.true_val
        else
            if not types.boolean.false_val then
                types.boolean.false_val =
                    setmetatable({v = false},  types.boolean_meta)
            end

            return types.boolean.false_val
        end
    end
}

types.boolean_meta = {
    __tostring = function(self)
        if self.v then
            return "#t"
        else
            return "#f"
        end
    end,

    __index = setmetatable({
        type = "boolean",

        eval = function(self, env)
            return self
        end
    }, types.base_meta)
}


types.pair = {
    new = function(a, b)
        return setmetatable({a, b}, types.pair_meta)
    end
}

types.pair_meta = {
    __tostring = function(self)
        local v2s = tostring(self[2])

        if self[2].type == "pair" then
            v2s = v2s:sub(2, #v2s - 1)
            return ("(%s %s)"):format(self[1], v2s)
        else
            return ("(%s . %s)"):format(self[1], v2s)
        end

    end,

    __index = setmetatable({
        type = "pair",

        getval = function(self)
            return self
        end,

        eval = function(self, env)
            return self
        end
    }, types.base_meta)
}


types.list = {
    new = function(list)
        if not list or #list == 0 then
            if not types.list.empty_list then
                types.list.empty_list = setmetatable({}, types.list_meta)
            end

            return types.list.empty_list
        else
            return setmetatable(list, types.list_meta)
        end
    end
}

types.list_meta = {
    __tostring = function(self)
        local ss = {}

        for _, v in ipairs(self) do
            table.insert(ss, tostring(v))
        end

        return "(" .. table.concat(ss, " ") .. ")"
    end,

    __index = setmetatable({
        type = "list",

        getval = function(self)
            return self
        end,

        eval = function(self, env)
            -- Evaluating a list is always a function call.
            local head = self[1]

            -- Empty list evaluates to itself
            if not head then
                return self
            end

            local tail = {table.unpack(self, 2)}

            -- Look at head to find out what we need to do.
            if head.type == "procedure" then
                -- Best case, we already got our callable, nothing to do
            elseif head.type == "identifier" then
                -- Next best case, we have something to look up or it's a
                -- special form.
                local name_lower = head:getval():lower()

                if special_forms[name_lower] then
                    return special_forms[name_lower](head, env, tail)
                end

                local target = head:eval(env)

                ensure(head, target.type == "procedure",
                    "bad-invoke-error",
                    "cannot invoke on %s of type %s",
                        target,
                        target.type)

                target:setevalpos(head:getpos())
                head = target

            elseif head.type == "list" then
                -- Annoying case, another function call or the empty list
                local res = head:eval(env)

                ensure(head, res.type == "procedure",
                    "bad-invoke-error",
                    "cannot invoke on %s of type %s",
                        res,
                        res.type)

                res:setevalpos(head:getpos())
                head = res

            else
                err(head,
                    "bad-invoke-error",
                    "cannot invoke on %s of type %s",
                        head,
                        head.type)
            end

            for i, arg in ipairs(tail) do
                local argv = arg:eval(env)
                argv:setevalpos(arg:getpos())

                tail[i] = argv
            end

            return head:call(env, tail)
        end
    }, types.base_meta)
}


types.proc = {
    new = function(name, parent_env, params, variadic_param, body)
        return setmetatable({
            name   = name,
            env    = parent_env:derive(name),
            params = params,
            body   = body,

            varparam = variadic_param,
            builtin  = false
        }, types.proc_meta)
    end,

    new_builtin = function(name, func)
        return setmetatable({
            name   = name,
            env    = nil,
            params = nil,
            body   = func,

            varparam = nil,
            builtin  = true
        }, types.proc_meta)
    end,

    wrap_native = function(name, wrapped_function)
        local function wrapper(self, env, args)
            local n = #args

            for i = 1, #args do
                args[i] = types.tolua(args[i], env)
            end

            local res = {xpcall(function()
                return wrapped_function(table.unpack(args, 1, n))

            end, function(err)
                local f, l, c = self:getpos()
                local location = { file = f, line = l, col  = c }

                return types.err.new("lua-error", location,
                    ("in Lua function %s: %s"):format(name, err))
            end)}

            if res[1] then
                if #res > 2 then
                    return types.toscheme({table.unpack(res, 2)})
                else
                    return types.toscheme(res[2])
                end
            else
                error(res[2])
            end
        end

        return setmetatable({
            name   = name,
            env    = nil,
            params = nil,
            body   = wrapper,

            varparam = nil,
            builtin  = true,
            wraps    = wrapped_function
        }, types.proc_meta)
    end
}

types.proc_meta = {
    __tostring = function(self)
        local def = ""

        if self:getpos() then
            def = (" defined at %s:%d:%d"):format(self:getpos())
        end

        if self.name then
            if self.builtin then
                return ("#<native-procedure %q%s>"):format(self.name, def)
            else
                return ("#<procedure %q%s>"):format(self.name, def)
            end
        else
            return ("#<anonymous-procedure%s>"):format(def)
        end
    end,

    __index = setmetatable({
        type = "procedure",

        call = function(self, env, args)
            if type(self.body) == "function" then
                -- builtin, just pass it the raw arguments
                return self.body(self, env, args)
            end

            expect_argc_min(self, #self.params, #args)

            if not self.varparam then
                -- Not variadic, also has an upper bound
                expect_argc_max(self, #self.params, #args)

                for i, arg in ipairs(args) do
                    self.env:define(self.params[i], arg)
                end
            else
                local vargs = {}

                for i, arg in ipairs(args) do
                    if i > #self.params then
                        table.insert(vargs, arg)
                    else
                        self.env:define(self.params[i], arg)
                    end
                end

                self.env:define(self.varparam, types.list.new(vargs))
            end

            return self.body:eval(self.env)
        end
    }, types.base_meta)
}


types.port = {
    new = function(file, path, mode)
        return setmetatable({
            v = file,
            path = path,
            mode = mode
        }, types.port_meta)
    end,

    wrap_native = function(fileobj)
        return setmetatable({
            v = fileobj,
            path = "<unknown path>",
            mode = "?"
        }, types.port_meta)
    end,

    eof_object = setmetatable({}, {
        __tostring = function(self)
            return "#<eof>"
        end,

        __index = setmetatable({
            type = "eof-object",

            eval = function(self, env)
                return self
            end
        }, types.base_meta)
    })
}

types.port_meta = {
    __tostring = function(self)
        local mode

        if self.mode == "r" then
            mode = "input-"
        elseif self.mode == "w" then
            mode = "output-"
        else
            mode = "native-"
        end

        return ("#<%sport %q>"):format(mode, self.path)
    end,

    __index = setmetatable({
        type = "port",

        eval = function(self, env)
            return self
        end,

        is_open = function(self)
            return self.v ~= nil
        end,

        close = function(self)
            self.v:close()
            self.v = nil
        end
    }, types.base_meta)
}


types.err = {
    new = function(typ, pos, message, lvl)
        return setmetatable({
            errtype = typ,
            errpos  = pos,
            errmsg  = message,
            errtrace = debug.traceback(nil, lvl or 2)
        }, types.err_meta)
    end
}

types.err_meta = {
    __tostring = function(self)
        if self.errpos then
            return ("#<%s: %s:%d:%d: %s>"):format(
                self.errtype,
                self.errpos.file,
                self.errpos.line,
                self.errpos.col,
                self.errmsg)
        else
            return ("#<%s: %s>"):format(self.errtype, self.errmsg)
        end
    end,

    __index = setmetatable({
        type = "error",

        eval = function(self, env)
            return self
        end,

        tostring = function(self)
            if self.errpos then
                return ("%s:%d:%d: %s: %s\n%s"):format(
                    self.errpos.file,
                    self.errpos.line,
                    self.errpos.col,
                    self.errtype,
                    self.errmsg,
                    self.errtrace)
            else
                return ("?:?:?: %s: %s\n%s"):format(
                    self.errtype,
                    self.errmsg,
                    self.errtrace)
            end
        end
    }, types.base_meta)
}


types.map = {
    new = function(map)
        return setmetatable({
            v = map
        }, types.map_meta)
    end
}

types.map_meta = {
    __tostring = function(self)
        return ("#<lua-map: %s>"):format(self.v)
    end,

    __index = setmetatable({
        type = "map",

        eval = function(self, env)
            return self
        end,
    }, types.base_meta)
}

types.lua_nil = setmetatable({}, {
    __tostring = function(self)
        return "#<nil>"
    end,

    __index = setmetatable({
        type = "nil",

        eval = function(self, env)
            return self
        end
    }, types.base_meta)
})

-- Slightly derived from: https://stackoverflow.com/a/7528301/280656
local function is_array(tab)
    if type(tab) ~= "table" then
        return false
    end

    local count = 0

    for k, v in pairs(tab) do
        if type(k) ~= "number" then
            return false
        else
            count = count + 1
        end
    end

    for i = 1, count do
        if not tab[i] and type(tab[i]) ~= "nil" then
            return false
        end
    end

    return true
end

function types.toscheme(val)
    -- Might be a scheme value we're receiving from Lua, due to bidirectional
    -- interop -> check metatables
    local valmt = getmetatable(val)

    if valmt then
        if valmt == types.symbol_meta or
           valmt == types.ident_meta or
           valmt == types.str_meta or
           valmt == types.number_meta or
           valmt == types.char_meta or
           valmt == types.boolean_meta or
           valmt == types.pair_meta or
           valmt == types.list_meta or
           valmt == types.proc_meta or
           valmt == types.port_meta or
           valmt == types.err_meta or
           val == types.lua_nil then

            return val
        end
    end

    if type(val) == "table" then
        -- Can be an array or a map. We don't support maps yet, so we'll treat
        -- it as a list.
        if not is_array(val) then
            return types.map.new(val)
        end

        local t = {}
        local toscheme = types.toscheme

        for i, v in ipairs(val) do
            table.insert(t, toscheme(v))
        end

        return types.list.new(t)
    elseif type(val) == "string" then
        return types.str.new(val)
    elseif type(val) == "number" then
        return types.number.new(val)
    elseif type(val) == "boolean" then
        return types.boolean.new(val)
    elseif type(val) == "nil" then
        return types.lua_nil
    elseif type(val) == "userdata" and io.type(val) == "file" then
        return types.port.wrap_native(val)
    elseif type(val) == "function" then
        return types.proc.wrap_native("?", val)
    else
        error(("cannot convert value of type %s to scheme value: %s"):format(
            type(val), val), 2)
    end
end

function types.tolua(val, env)
    if val.type == "list" or val.type == "pair" then
        -- Not much of a distinction between pairs and lists in Lua land
        local list = {}
        local tolua = types.tolua

        for _, v in ipairs(val:getval()) do
            table.insert(list, tolua(v, env))
        end

        return list
    elseif val.type == "procedure" then
        -- We can wrap procedures inside a helper closure that converts the
        -- parameters and return values, the same way we can wrap a Lua function
        -- for Scheme.
        return function(...)
            if val.wraps then
                -- We know this function just wraps a Lua function, so we can
                -- call it directly and avoid all the overhead of translating.
                return val.wraps(...)
            else
                local args = {...}

                for i, v in ipairs(args) do
                    args[i] = types.toscheme(v, env)
                end

                return types.tolua(val:call(env, {table.unpack(args)}), env)
            end
        end

    elseif val.type == "error" then
        return tostring(val)

    elseif val.type == "nil" then
        return nil

    else
        -- Try getting a primitive (symbol, ident, str, number, char, bool) or
        -- the file object from a port.
        local v = val:getval()

        if not v then
            util.err(val,
                "type-error",
                "cannot convert value of type %s to Lua value: %s",
                    val.type,
                    val)
        end

        return v
    end
end

return types
