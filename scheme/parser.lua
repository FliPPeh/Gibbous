local parser = {}
local parser_methods = {}

local stringparser_methods = setmetatable({}, { __index = parser_methods })
local fileparser_methods = setmetatable({}, { __index = parser_methods })

local types = require "scheme.types"


local function parser_new(source)
    return {
        source = source,
        line = 1,
        col = 1,

        store_lexical_information = true
    }
end

--[[
-- String parser methods
--]]
function parser.new_from_string(str)
    local self = parser_new("<string>")

    self.input = str or ""
    self.pos = 1

    return setmetatable(self, {
        __index = stringparser_methods
    })
end

function stringparser_methods:get_char()
    return self.input:sub(self.pos, self.pos)
end

function stringparser_methods:advance()
    self:update_linecol()

    self.pos = self.pos + 1
end

function stringparser_methods:feed(chunk)
    self.input = self.input .. chunk
end

--[[
-- File parser methods
--]]
function parser.new_from_file(f)
    local self = parser_new(f)

    self.file = io.open(f, "r")
    self.bufsiz = 4096
    self.buf = nil
    self.bufpos = 1

    return setmetatable(self, {
        __index = fileparser_methods
    })
end

function parser.new_from_open_file(fp, name)
    local self = parser_new(name)

    self.file = fp
    self.bufsiz = 1 -- much more expensive, but we don't want to read more than
                    -- what's necessary in case someone else wants to read from
                    -- the same handle.
    self.buf = nil
    self.bufpos = 1
    self. store_lexical_information = false

    return setmetatable(self, {
        __index = fileparser_methods
    })
end

function fileparser_methods:get_char()
    if self.buf == nil then
        self.buf = self.file:read(self.bufsiz)
    end

    return self.buf:sub(self.bufpos, self.bufpos)
end

function fileparser_methods:advance()
    self:update_linecol()

    self.bufpos = self.bufpos + 1
    if self.bufpos > #self.buf then
        self.buf = self.file:read(self.bufsiz) or ""
        self.bufpos = 1
    end
end

--[[
-- Generic methods
--]]
function parser_methods:update_linecol()
    if self:get_char() == "\n" then
        self.line = self.line + 1
        self.col  = 1
    else
        self.col = self.col + 1
    end
end

function parser_methods:err(fmt, ...)
    error(types.err.new("parse-error", {
        file = self.source,
        line = self.line,
        col  = self.col
    }, fmt:format(...)))
end

function parser_methods:emit(fn, v, dl, dc)
    local v = fn(v)

    if self.store_lexical_information then
        v:setpos(self.source, dl, dc)
    end

    return v
end

function parser_methods:trim(c)
    c = c or self:get_char()

    while c and c:find("^%s") do
        self:advance()

        c = self:get_char()
    end

    return c
end

function parser_methods:parse()
    local list = {}

    while true do
        local res = self:parse_value()

        if res then
            table.insert(list, res)
        else
            break
        end
    end

    return list
end

function parser_methods:parse_value(c)
    if not c or c:find("^%s") then
        c = self:trim()
    end

    if not c or c == "" then
        return nil
    end

    if c == ";" then
        self:advance()

        c = self:get_char()
        while c ~= "\n" and c ~= "" do
            self:advance()

            c = self:get_char()
        end

        if c ~= "" then
            return self:parse_value(c)
        end

    elseif c == "(" then
        -- list
        return self:parse_list()

    elseif c == "#" then
        -- boolean, character
        return self:parse_bool_or_char()

    elseif c == "\"" then
        -- string
        return self:parse_string()

    elseif not c:find("[%d%s%(%)%#%[%]%'%;]") then
        -- identifier
        return self:parse_identifier(c)

    elseif c:find("[%+%-%d%.]") then
        return self:parse_number(c)

    elseif c == "'" then
        local dl, dc = self.line, self.col

        self:advance()

        return self:emit(types.list.new, {
            self:emit(types.ident.new, "quote", dl, dc),
            self:parse_value()}, dl, dc)
    else
        self:err("unexpected character: %q", c)
    end
end

function parser_methods:parse_list()
    local dl, dc = self.line, self.col

    -- Skip past "("
    self:advance()

    local list = {}
    local c

    while true do
        c = self:trim()

        if c == ")" or c == "" then
            break
        else
            table.insert(list, self:parse_value(c))
        end
    end

    if c ~= ")" then
        -- Hit EOF
        self:err("expected \")\", found <eof> instead")
    end

    self:advance()

    return self:emit(types.list.new, list, dl, dc)
end

function parser_methods:parse_bool_or_char()
    local dl, dc = self.line, self.col

    self:advance()

    local c = self:get_char()

    if c == "t" or c == "T" then
        self:advance()

        return self:emit(types.boolean.new, true, dl, dc)

    elseif c == "f" or c == "F" then
        self:advance()

        return self:emit(types.boolean.new, false, dl, dc)

    elseif c == "\\" then
        self:advance()

        c = self:get_char()

        if c == "" then
            self:err("expected character, found <eof>")
        end

        if not c:find("%s") and not c:find("%w") then
            self:advance()

            return self:emit(types.char.new, c, dl, dc)
        else
            local buf = c

            self:advance()

            while true do
                c = self:get_char()

                if not c:find("%a") and not c:find("%x") then
                    break
                else
                    buf = buf .. c

                    self:advance()
                end
            end

            if #buf == 1 then
                return self:emit(types.char.new, buf, dl, dc)
            elseif #buf > 1 then
                if buf:find("x%x%x") then
                    return self:emit(
                        types.char.new,
                        string.char(tonumber(buf:sub(2), 16)),
                        dl,
                        dc)
                else
                    local res = types.charnames[buf]

                    if not res then
                        self:err("unknown character code: %s", buf)
                    end

                    return self:emit(types.char.new, res, dl, dc)
                end
            end
        end
    end
end

function parser_methods:parse_string(c)
    local buf = ""

    local dl, dc = self.line, self.col

    self:advance()

    c = self:get_char()

    while c ~= "\"" and c ~= "" do
        if c == "\\" then
            self:advance()

            local esc = { ["\""] = "\"", t = "\t", n = "\n", r = "\r" }
            c = self:get_char()

            if esc[c] then
                buf = buf .. esc[c]
                self:advance()
            else
                self:err("invalid escape sequence \"%s\"", "\\" .. c)
            end
        else
            buf = buf .. c
            self:advance()
        end

        c = self:get_char()
    end

    if c ~= "\"" then
        -- Hit EOF
        self:err("expected end of string, found <eof> instead")
    end

    self:advance()

    return self:emit(types.str.new, buf, dl, dc)
end

function parser_methods:parse_identifier(c)
    local buf = c

    local dl, dc = self.line, self.col

    self:advance()

    while true do
        c = self:get_char()

        if not c:find("[%s%(%)%#%[%]%'%;]") then
            buf = buf .. c

            self:advance()
        else
            break
        end
    end

    if tonumber(buf) ~= nil then
        return self:emit(types.number.new, tonumber(buf), dl, dc)
    else
        return self:emit(types.ident.new, buf, dl, dc)
    end
end

function parser_methods:parse_number(n)
    local buf = n

    local dl, dc = self.line, self.col

    self:advance()

    while true do
        c = self:get_char()

        if not c:find("[%.%deE]") then
            break
        else
            buf = buf .. c

            self:advance()
        end
    end

    local num = tonumber(buf)

    if not num then
        self.line, self.col = dl, dc

        self:err("invalid number: %s", buf)
    else
        return self:emit(types.number.new, num, dl, dc)
    end
end

return parser
