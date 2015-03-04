local types = {}

local util = require "scheme.util"
local special_forms = require "scheme.special_forms"

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
            util.err(self, "cannot evaluate value of type %s: %s",
                self:type(),
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
        type = function(self)
            return "symbol"
        end,

        eval = function(self, env)
            return self
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
        type = function(self)
            -- Shouldn't see this from within Scheme.
            return "identifier"
        end,

        eval = function(self, env)
            local val = self

            while val:type() == "identifier" do
                nval = env:resolve(val:getval())

                if not nval then
                    util.err(self, "unresolved procedure or variable: %s",
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
        type = function(self)
            return "string"
        end,

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
        type = function(self)
            return "number"
        end,

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
        type = function(self)
            return "char"
        end,

        eval = function(self, env)
            return self
        end
    }, types.base_meta)
}


types.boolean = {
    new = function(bool)
        return setmetatable({
            v = bool
        }, types.boolean_meta)
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
        type = function(self)
            return "boolean"
        end,

        eval = function(self, env)
            return self
        end
    }, types.base_meta)
}


types.list = {
    new = function(list)
        return setmetatable({
            v = list
        }, types.list_meta)
    end
}

types.list_meta = {
    __tostring = function(self)
        local ss = {}

        for _, v in ipairs(self.v) do
            table.insert(ss, tostring(v))
        end

        return "(" .. table.concat(ss, " ") .. ")"
    end,

    __index = setmetatable({
        type = function(self)
            return "list"
        end,

        car = function(self)
            return self:getval()[1]
        end,

        cdr = function(self)
            return {table.unpack(self:getval(), 2)}
        end,

        eval = function(self, env)
            -- Evaluating a list is always a function call.
            local head = self:car()

            -- Empty list evaluates to itself
            if not head then
                return self
            end

            local tail = self:cdr()

            -- Save source position so error messages point to this atom,
            -- rather than the place where the value it's holding was
            -- defined.
            local df, dl, dc = head:getpos()

            -- Look for something callable, or something that can be
            -- resolved to something callable.
            while head:type() ~= "identifier" and head:type() ~= "procedure" do
                head:setevalpos(df, dl, dc)

                -- Can it be evaluated directly or looked up further?
                if          head:type() ~= "list"
                        and head:type() ~= "procedure"
                        and head:type() ~= "identifier" then
                    util.err(head, "cannot invoke on value of type %s: %s",
                        head:type(),
                        head)

                elseif head:type() == "list" and #head:getval() == 0 then
                    util.err(head, "cannot invoke on empty list")

                elseif head:type() == "list" and
                       head:getval()[1]:type() == "identifier" and
                       head:getval()[1]:getval() == "quote" then
                    util.err(head, "cannot invoke on quoted list: %s",
                        head:getval()[2])

                end

                head = head:eval(env)
            end

            -- Atom, resolve to function or special form.
            if head:type() == "identifier" then
                -- Is it a special form?
                if special_forms[head:getval()] then
                    return special_forms[head:getval()](head, env, tail)

                else
                    -- Have something to look up, so look it up.
                    while head:type() ~= "procedure" do
                        head = head:eval(env)
                    end

                    head:setevalpos(df, dl, dc)
                end
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
    end
}

types.proc_meta = {
    __tostring = function(self)
        return ("(procedure \"%s\" (%s) %s)"):format(
            self.name or "lambda",
            table.concat(self.params, " "),
            self.body)
    end,

    __index = setmetatable({
        type = function(self)
            return "procedure"
        end,

        call = function(self, env, args)
            if type(self.body) == "function" then
                -- builtin, just pass it the raw arguments
                local evargs = {}

                for i, arg in ipairs(args) do
                    local argv = arg:eval(env)
                    argv:setevalpos(arg:getpos())

                    table.insert(evargs, argv)
                end

                return self.body(self, evargs)
            end

            util.expect_argc_min(self, #self.params, #args)

            if not self.varparam then
                -- Not variadic, also has an upper bound
                util.expect_argc_max(self, #self.params, #args)

                for i, arg in ipairs(args) do
                    local argv = arg:eval(env)
                    argv:setevalpos(arg:getpos())

                    self.env:define(self.params[i], argv)
                end
            else
                local vargs = types.list.new{}

                for i, arg in ipairs(args) do
                    local argv = arg:eval(env)
                    argv:setevalpos(arg:getpos())

                    if i > #self.params then
                        table.insert(vargs:getval(), argv)
                    else
                        self.env:define(self.params[i], argv)
                    end
                end

                self.env:define(self.varparam, vargs)
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
    end
}

types.port_meta = {
    __tostring = function(self)
        return ("(%s-file %q)"):format(
            self.mode == "r" and "input" or "output",
            self.path)
    end,

    __index = setmetatable({
        type = function(self)
            return "port"
        end,

        eval = function(self, env)
            return self
        end
    }, types.base_meta)
}


function types.toscheme(val)
    if type(val) == "table" then
        return types.list.new(val)
    elseif type(val) == "string" then
        return types.str.new(val)
    elseif type(val) == "number" then
        return types.number.new(val)
    elseif type(val) == "boolean" then
        return types.boolean.new(val)
    elseif type(val) == "nil" then
        return types.list.new{}
    else
        error(("cannot convert value of type %s to scheme value: %s"):format(
            type(val), val), 2)
    end
end

function types.tolua(val)
    if val:type() == "list" then
        local list = {}

        for _, v in ipairs(val:getval()) do
            table.insert(list, types.tolua(v))
        end

        return list
    else
        return val:getval()
    end
end

return types
