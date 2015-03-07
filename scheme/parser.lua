local parser = {}
local parser_meta = {}

local types = require "scheme.types"


function parser.new(file)
    return setmetatable({
        input = "",
        pos   = 1,
        line  = 1,
        col   = 1,
        file  = file or "input"
    }, {
        __index = parser_meta,

        __tostring = function(self)
            return "parser"
        end
    })
end


function parser_meta:is_eof()
    return self.pos > #self.input
end

function parser_meta:advance(n)
    for i = 0, n - 1 do
        if self.input:sub(self.pos + i, self.pos + i) == "\n" then
            self.line = self.line + 1
            self.col  = 1
        else
            self.col = self.col + 1
        end
    end

    self.pos = self.pos + n
end

local function is_valid_ident(c)
    return not c:find("[%s%(%)%#%[%]%'%;]")
end

local function is_valid_ident_start(c)
    -- return not c:find("[%s%(%)%#%[%]%'%.]")
    return is_valid_ident(c)
end


local function is_number(str)
    return tonumber(str) ~= nil
end

function parser_meta:err(fmt, ...)
    error(types.err.new("parse-error", {
        file = self.file,
        line = self.line,
        col  = self.col
    }, fmt:format(...)), 0)
end

function parser_meta:expect(c)
    if self:is_eof() then
        self:err("expected %q, found <eof>", c)
    elseif self:getc() == c then
        self:advance(1)
    else
        self:err("invalid character %q (expected %q)", self:getc(), c)
    end
end

function parser_meta:skip_whitespace()
    local i, j = self.input:sub(self.pos):find('^%s+')

    if i and j then
        self:advance(j)
    end
end

function parser_meta:skip_to_next_line()
    while not self:is_eof() and self:getc() ~= "\n" do
        self:advance(1)
    end
end

function parser_meta:getc()
    return self.input:sub(self.pos, self.pos)
end

function parser_meta:emit(ctor, val, defline, defcol)
    local v = ctor(val)
    v:setpos(self.file, defline, defcol)

    return v
end

function parser_meta:parse_list()
    local list = {}

    local getc = self.getc
    local is_eof = self.is_eof
    local expect = self.expect
    local skip_whitespace = self.skip_whitespace
    local parse_value = self.parse_value

    expect(self, "(")
    skip_whitespace(self)

    while not is_eof(self) and getc(self) ~= ")" do
        table.insert(list, parse_value(self))

        skip_whitespace(self)
    end

    expect(self, ")")

    return list
end

function parser_meta:parse_ident()
    local buf = ""

    local getc = self.getc
    local is_eof = self.is_eof
    local expect = self.expect

    local c = getc(self)

    while not is_eof(self) and is_valid_ident(c) do
        buf = buf .. c

        expect(self, c)
    end

    if is_number(buf) then
        return tonumber(buf)
    else
        return buf
    end
end

function parser_meta:parse_string()
    local buf = ""

    local getc = self.getc
    local is_eof = self.is_eof
    local expect = self.expect

    expect(self, "\"")

    while not is_eof(self) and getc(self) ~= "\"" do
        local c = getc(self)

        if c == "\\" then
            expect(self, "\\")

            local esc = {
                ["\""] = "\"", t = "\t", n = "\n", r = "\r"
            }

            local c = getc(self)

            if esc[c] then
                buf = buf .. esc[c]
                expect(self, c)
            else
                self:err("invalid escape sequence \"%s\"", "\\" .. c)
            end
        else
            buf = buf .. c

            expect(self, c)
        end

    end

    expect(self, "\"")

    return buf
end


function parser_meta:parse_value()
    self:skip_whitespace()

    -- Save a few functions as locals to save a few lookups
    local getc = self.getc
    local emit = self.emit
    local is_eof = self.is_eof
    local expect = self.expect

    local c = getc(self)

    if c == "(" then
        local dl = self.line
        local dc = self.col

        return emit(self, types.list.new, self:parse_list(), dl, dc)

    elseif c == ";" then
        self:skip_to_next_line()

        if not is_eof(self) then
            return self:parse_value()
        end

    elseif c == "\"" then
        local dl = self.line
        local dc = self.col

        return emit(self, types.str.new, self:parse_string(), dl, dc)

    --[[
    elseif c == "." then
        local dl = self.line
        local dc = self.col

        expect(self, ".")
        return emit(self, types.mkident, ".", dl, dl)
    --]]
    elseif is_valid_ident_start(c) then
        local ident = ""
        local dl = self.line
        local dc = self.col

        while not is_eof(self) and is_valid_ident(getc(self)) do
            ident = ident .. getc(self)

            expect(self, getc(self))
        end

        if is_number(ident) then
            return emit(self, types.number.new, tonumber(ident), dl, dc)
        else
            return emit(self, types.ident.new, ident, dl, dc)
        end

    elseif c == "'" then
        local dl = self.line
        local dc = self.col

        expect(self, c)

        return emit(self, types.list.new, {
            emit(self, types.ident.new, "quote", dl, dc),
            self:parse_value()}, dl, dc)

    elseif c == "#" then
        local dl = self.line
        local dc = self.col

        expect(self, c)

        local c = getc(self)

        if c == "t" or c == "T" then
            expect(self, c)

            return emit(self, types.boolean.new, true, dl, dc)
        elseif c == "f" or c == "F" then
            expect(self, c)

            return emit(self, types.boolean.new, false, dl, dc)
        elseif c == "\\" then
            expect(self, c)

            local char = getc(self)
            if not char:find("%s") and not char:find("%w") then
                -- Not alphanumeric, done here
                expect(self, char)

                return emit(self, types.char.new, char, dl, dc)
            else
                expect(self, char)

                if char == "x" then
                    -- Hex insert? Maybe not done here.
                    local la = self.input:sub(self.pos, self.pos + 1)

                    if la:find("%x%x") then
                        expect(self, la:sub(1, 1))
                        expect(self, la:sub(2, 2))

                        return emit(self, types.char.new,
                            string.char(tonumber(la, 16)), dl, dc)
                    else
                        return emit(self, types.char.new, char, dl, dc)
                    end
                else
                    local buf = char
                    local c = getc(self)

                    while not is_eof(self) and c:find('%w') do
                        buf = buf .. c

                        expect(self, c)

                        c = getc(self)
                    end

                    if #buf > 1 then
                        if not types.charnames[buf] then
                            self.col = self.col - #buf
                            self:err("unknown character name: %s", buf)
                        else
                            return emit(self, types.char.new,
                                types.charnames[buf], dl, dc)
                        end
                    else
                        return emit(self, types.char.new, buf, dl, dc)
                    end
                end
            end

        else
            self:err("unexpected token %q following \"#\"", c)
        end
    else
        self:err("unexpected token %q", getc(self))
    end
end

function parser_meta:feed(input)
    self.input = self.input .. input
end

function parser_meta:parse(input)
    local ast = {}

    if input then
        self:feed(input)
    end

    self:skip_whitespace()

    while not self:is_eof() do
        local val = self:parse_value()

        if val then
            table.insert(ast, val)
        end

        self:skip_whitespace()
    end

    return ast
end

return parser
