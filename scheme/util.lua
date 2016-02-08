local util = {}

function util.err(subj, t, fmt, ...)
    local types = require "scheme.types"
    local file, line, col = subj:getpos()

    local msg = (type(fmt) == "string" and fmt:format(...)) or fmt

    if file and line and col then
        local position = {
            file = file,
            line = line,
            col  = col
        }

        -- error(("%s:%d:%d: "):format(file, line, col) .. fmt:format(...), 0)
        error(types.err.new(t, position, msg), 0)
    else
        -- error("?:?:?: " .. fmt:format(...), 0)
        error(types.err.new(t, nil, msg), 0)
    end
end

function util.expect_argc_min(var, n, have)
    local name = var.name and (var.name .. ": ") or ""

    if have < n then
        util.err(var, "argument-error", name ..
            "too few arguments: expected at least %d; got %d",
                n, have)
    end
end

function util.expect_argc_max(var, n, have)
    local name = var.name and (var.name .. ": ") or ""

    if have > n then
        util.err(var, "argument-error", name ..
            "too many arguments: expected at most %d; got %d",
                n, have)
    end
end

function util.expect_argc(var, n, have)
    util.expect_argc_min(var, n, have)
    util.expect_argc_max(var, n, have)
end

function util.expect(var, typ, as)
    as = as and (as .. ": ") or ""

    if type(typ) == "string" then
        if var.type ~= typ then
            util.err(var, "type-error",
                as ..  "expected value of type %s; got: %s (%s)",
                    typ,
                    tostring(var),
                    var.type)
        end
    else
        for _, t in ipairs(typ) do
            if var.type == t then
                return nil
            end
        end

        util.err(var, "type-error",
            as .. "expected value of either type %s; got: %s (%s)",
                table.concat(typ, ", "),
                tostring(var),
                var.type)
    end
end

function util.not_implemented(var, fmt, ...)
    util.err(var, "not-implemented",
        fmt and fmt:format(...) or "not implemented")
end

function util.ensure(var, cond, t, fmt, ...)
    if not cond then
        util.err(var, t, fmt:format(...))
    end
end

function util.is_true(val)
    if val.type ~= "boolean" or val:getval() then
        return true
    else
        return false
    end
end

function util.split_string(str, sep)
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

function util.literal_pattern(str)
    return str:gsub("[^%w%s]", "%%%1")
end

function util.display_repr(arg)
    if arg.type == "symbol" or
       arg.type == "string" or
       arg.type == "number" or
       arg.type == "char" then

       return arg:getval()

    elseif arg.type == "list" then
        local parts = {}

        for i, v in ipairs(arg:getval()) do
            table.insert(parts, util.display_repr(v))
        end

        return "(" .. table.concat(parts, " ") .. ")"
    elseif arg.type == "pair" then
        local v2s = util.display_repr(arg:getval()[2])

        if arg:getval()[2].type == "pair" then
            v2s = v2s:sub(2, #v2s - 1)
            return ("(%s %s)"):format(util.display_repr(arg:getval()[1]), v2s)
        else
            return ("(%s . %s)"):format(util.display_repr(arg:getval()[1]), v2s)
        end
    else
        return tostring(arg)
    end
end

function util.write_repr(arg)
	return tostring(arg)
end

function util.desymbolize(val)
    local types = require "scheme.types"

    if val.type == "symbol" then
        local i = types.ident.new(val:getval())

        i:setpos(val:getpos())
        return i
    elseif val.type == "list" then
        for i, v in ipairs(val:getval()) do
            val:getval()[i] = util.desymbolize(v)
        end

        return val
    else
        return val
    end
end

return util
