local util = {}

function util.err(subj, fmt, ...)
    local line, col = subj:getpos()

    if line and col then
        error(("<in>:%d:%d: "):format(line, col) .. fmt:format(...), 0)
    else
        error("<in>:?:?: " .. fmt:format(...), 0)
    end
end

function util.expect_argc_min(var, n, have)
    name = var:getval() and (var:getval() .. ": ") or ""

    if have < n then
        util.err(var, name ..
            "too few arguments: expected at least %d; got %d",
                n, have)
    end
end

function util.expect_argc_max(var, n, have)
    name = var:getval() and (var:getval() .. ": ") or ""

    if have > n then
        util.err(var, name ..
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
            util.err(var, as ..  "expected value of type %s; got: %s (%s)",
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

        util.err(var, as .. "expected value of either type %s; got: %s (%s)",
            table.concat(typ, ", "),
            var,
            var:type())
    end
end

function util.not_implemented(var)
    util.err(var, "not implemented")
end

function util.ensure(var, cond, fmt, ...)
    if not cond then
        util.err(var, fmt:format(...))
    end
end

return util
