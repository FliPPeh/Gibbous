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

local function is_valid_atom(c)
    return not c:find("[%s%(%)%#%[%]%']")
end

local function is_valid_atom_start(c)
    -- return not c:find("[%s%(%)%#%[%]%'%.]")
    return is_valid_atom(c)
end


local function is_number(str)
    return tonumber(str) ~= nil
end

function parser_meta:err(fmt, ...)
    error((("<in>:%d:%d: "):format(self.line, self.col)) .. fmt:format(...), 0)
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

function parser_meta:getc()
    return self.input:sub(self.pos, self.pos)
end

function parser_meta:emit(ctor, val, defline, defcol)
    local v = ctor(val)
    v:setpos(defline, defcol)

    return v
end

function parser_meta:parse_list()
    local list = {}

    self:expect("(")
    self:skip_whitespace()

    while not self:is_eof() and self:getc() ~= ")" do
        table.insert(list, self:parse_value())

        self:skip_whitespace()
    end

    self:expect(")")

    return list
end

function parser_meta:parse_atom()
    local buf = ""

    while not self:is_eof() and is_valid_atom(self:getc()) do
        buf = buf .. self:getc()

        self:expect(self:getc())
    end

    if is_number(buf) then
        return tonumber(buf)
    else
        return buf
    end
end

function parser_meta:parse_string()
    local buf = ""

    self:expect("\"")

    while not self:is_eof() and self:getc() ~= "\"" do
        local c = self:getc()

        if c == "\\" then
            self:expect("\\")

            local esc = {
                ["\""] = "\"", t = "\t", n = "\n", r = "\r"
            }

            if esc[self:getc()] then
                buf = buf .. esc[self:getc()]

                self:expect(self:getc())
            else
                self:err("invalid escape sequence \"%s\"", "\\" .. self:getc())
            end
        else
            buf = buf .. c

            self:expect(c)
        end

    end

    self:expect("\"")

    return buf
end


function parser_meta:parse_value()
    self:skip_whitespace()

    local c = self:getc()

    if c == "(" then
        local dl = self.line
        local dc = self.col

        return self:emit(types.list.new, self:parse_list(), dl, dc)

    elseif c == "\"" then
        local dl = self.line
        local dc = self.col

        return self:emit(types.str.new, self:parse_string(), dl, dc)

    --[[
    elseif c == "." then
        local dl = self.line
        local dc = self.col

        self:expect(".")
        return self:emit(types.mkatom, ".", dl, dl)
    --]]
    elseif is_valid_atom_start(c) then
        local atom = ""
        local dl = self.line
        local dc = self.col

        while not self:is_eof() and is_valid_atom(self:getc()) do
            atom = atom .. self:getc()

            self:expect(self:getc())
        end

        if is_number(atom) then
            return self:emit(types.number.new, tonumber(atom), dl, dc)
        else
            return self:emit(types.atom.new, atom, dl, dc)
        end

    elseif c == "'" then
        local dl = self.line
        local dc = self.col

        self:expect(c)

        return self:emit(types.list.new,
            {types.atom.new("quote"), self:parse_value()}, dl, dc)

    elseif c == "#" then
        local dl = self.line
        local dc = self.col

        self:expect(c)

        local c = self:getc()

        if c == "t" or c == "T" then
            self:expect(c)

            return self:emit(types.boolean.new, true, dl, dc)
        elseif c == "f" or c == "F" then
            self:expect(c)

            return self:emit(types.boolean.new, false, dl, dc)
        elseif c == "\\" then
            self:expect(c)

            local char = self:getc()
            if not char:find("%s") and not char:find("%w") then
                -- Not alphanumeric, done here
                self:expect(char)

                return self:emit(types.char.new, char, dl, dc)
            else
                self:expect(char)

                if char == "x" then
                    -- Hex insert? Maybe not done here.
                    local la = self.input:sub(self.pos + 1, self.pos + 2)

                    self:expect(char)

                    if la:find("%x%x") then
                        self:expect(la:sub(1, 1))
                        self:expect(la:sub(2, 2))

                        return self:emit(types.char.new,
                            string.char(tonumber(la, 16)), dl, dc)
                    else
                        return self:emit(types.char.new, char, dl, dc)
                    end
                else
                    local buf = char

                    while not self:is_eof() and self:getc():find('%w') do
                        buf = buf .. self:getc()

                        self:expect(self:getc())
                    end

                    if #buf > 1 then
                        if not types.charnames[buf] then
                            self.col = self.col - #buf
                            self:err("unknown character name: %s", buf)
                        else
                            return self:emit(types.char.new,
                                types.charnames[buf], dl, dc)
                        end
                    else
                        return self:emit(types.char.new, buf, dl, dc)
                    end
                end
            end

        else
            self:err("unexpected token %q following \"#\"", c)
        end
    else
        self:err("unexpected token %q", self:getc())
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
        table.insert(ast, self:parse_value())

        self:skip_whitespace()
    end

    return ast
end

return parser
