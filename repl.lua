local env = require "scheme.env"

env = env.new_environment()

-- Quick and dirty way to find out if the chunk has balanced parentheses
local function is_balanced(str)
    local open = 0
    local close = 0

    local instr = false

    for i = 1, #str do
        if str:sub(i, i) == "\"" and str:sub(i - 1, i - 1) ~= "\\" then
            instr = not instr
        end

        if not instr then
            if str:sub(i, i) == "(" then
                open = open + 1
            elseif str:sub(i, i) == ")" then
                close = close + 1
            end
        end
    end

    return open == close
end

local evalbuf = ""
while true do
    if #evalbuf == 0 then
        io.write(("(input):%d > "):format(env.parser.line))
    else
        io.write(("        %d.. "):format(env.parser.line))
    end

    local line = io.read('*l')
    if not line then
        break
    end

    evalbuf = evalbuf .. line

    if is_balanced(evalbuf) then
        print(env:eval(evalbuf .. "\n"))

        evalbuf = ""
    end
end

print("^D - bye!")

