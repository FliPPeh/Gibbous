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
    name = var:getval() and (var:getval() .. ": ") or ""

    if have < n then
        util.err(var, "argument-error", name ..
            "too few arguments: expected at least %d; got %d",
                n, have)
    end
end

function util.expect_argc_max(var, n, have)
    name = var:getval() and (var:getval() .. ": ") or ""

    if have > n then
        util.err(var, "argument-error", name ..
            "too many arguments: expected %d; got %d",
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
        if var:type() ~= typ then
            util.err(var, "type-error",
                as ..  "expected value of type %s; got: %s (%s)",
                    typ,
                    var,
                    var:type())
        end
    else
        for _, t in ipairs(typ) do
            if var:type() == t then
                return nil
            end
        end

        util.err(var, "type-error",
            as .. "expected value of either type %s; got: %s (%s)",
                table.concat(typ, ", "),
                var,
                var:type())
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
    if val:type() ~= "boolean" or val:getval() then
        return true
    else
        return false
    end
end

return util
