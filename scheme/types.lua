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

types.base = {
    __index = {
        setpos = function(self, line, col)
            self.def_line = line
            self.def_col  = col
        end,

        getpos = function(self)
            return self.def_line, self.def_col
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
types.atom = {
    __tostring = function(self)
        return self.v
    end,

    __index = setmetatable({
        type = function(self)
            return "atom"
        end,

        eval = function(self, env)
            local val = self

            while val:type() == "atom" do
                nval = env:resolve(val:getval())

                if not nval then
                    util.err(self, "unresolved variable or function: %s",
                        val:getval())
                end

                val = nval
            end

            return val
        end

    }, types.base)
}

types.str = {
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
    }, types.base)
}

types.number = {
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
    }, types.base)
}

types.char = {
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
            return "character"
        end,

        eval = function(self, env)
            return self
        end
    }, types.base)
}

types.boolean = {
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
    }, types.base)
}

types.list = {
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
            local cdr = {}

            for i = 2, #self.v do
                table.insert(cdr, self:getval()[i])
            end

            return cdr
        end,

        eval = function(self, env)
            -- Evaluating a list is always a function call.

            local head = self:car()
            local tail = self:cdr()

            -- Save source position so error messages point to this atom,
            -- rather than the place where the value it's holding was
            -- defined.
            local dl, dc = head:getpos()

            -- Look for something callable, or something that can be
            -- resolved to something callable.
            while head:type() ~= "atom" and head:type() ~= "function" do
                head = head:eval(env)
            end

            -- Atom, resolve to function or special form.
            if head:type() == "atom" then
                -- Is it a special form?
                if special_forms[head:getval()] then
                    return special_forms[head:getval()](head, env, tail)

                else
                    -- Have something to look up, so look it up.
                    while head:type() ~= "function" do
                        head = head:eval(env)
                    end

                    head:setpos(dl, dc)
                end
            end

            return head:call(env, tail)
        end
    }, types.base)
}

types.func = {
    __tostring = function(self)
        return ("(function \"%s\" (%s) %s)"):format(
            self.name or "lambda",
            table.concat(self.params, " "),
            self.body)
    end,

    __index = setmetatable({
        type = function(self)
            return "function"
        end,

        call = function(self, env, args)
            util.expect_argc_min(self, #self.params, #args)

            if not self.varparam then
                -- Not variadic, also has an upper bound
                util.expect_argc_max(self, #self.params, #args)

                for i, arg in ipairs(args) do
                    local argv = arg:eval(env)
                    argv:setpos(arg:getpos())

                    self.env:define(self.params[i], argv)
                end
            else
                local vargs = types.mklist{}

                for i, arg in ipairs(args) do
                    local argv = arg:eval(env)
                    argv:setpos(arg:getpos())

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
    }, types.base)
}

--[[
-- Other types types
--]]
--
types.userdata = {
    __tostring = function(self)
        return ("<%s>"):format(self.v)
    end,

    __index = setmetatable({
        type = function(self)
            return "userdata"
        end,

        eval = function(self, env)
            return self
        end
    }, types.base)
}

function types.mklist(ls)    return setmetatable({v = ls},  types.list)    end
function types.mkstring(s)   return setmetatable({v = s},   types.str)     end
function types.mknumber(num) return setmetatable({v = num}, types.number)  end
function types.mkatom(at)    return setmetatable({v = at},  types.atom)    end
function types.mksymbol(s)   return setmetatable({v = s},   types.symbol)  end
function types.mkbool(b)     return setmetatable({v = b},   types.boolean) end
function types.mkchar(c)     return setmetatable({v = c},   types.char)    end

function types.mkfunction(env, params, body)
    return setmetatable({
        env = env,
        params = params,
        body = body,
        name = nil,
        varparam = nil
    }, types.func)
end

function types.mkluaval(ud)
    return setmetatable({v = ud}, types.luaval)
end

function types.toscheme(val, meta)
    if type(val) == "table" then
        return types.mklist(val)
    elseif type(val) == "string" then
        return types.mkstring(val)
    elseif type(val) == "number" then
        return types.mknumber(val)
    elseif type(val) == "boolean" then
        return types.mkbool(val)
    elseif type(val) == "nil" then
        return types.mklist{}
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
