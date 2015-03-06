local special_forms = {}

local util = require "scheme.util"

local err = util.err
local ensure = util.ensure
local expect = util.expect
local expect_argc = util.expect_argc
local expect_argc_min = util.expect_argc_min
local expect_argc_max = util.expect_argc_max
local is_true = util.is_true

--[[
special_forms["."] = function(self, env, args)
    local types = require "scheme.types"

    expect_argc_min(self, 1, #args)

    local head = args[1]

    while head:type() ~= "atom" do
        head = head:eval(env)
    end

    local val = env:locate_lua(head:getval())

    if val ~= nil and type(val) ~= "function" then
        expect_argc_max(self, 1, #args)

        return types.toscheme(val)
    elseif val ~= nil then
        local fargs = {}

        for i = 2, #args do
            fargs[i - 1] = types.tolua(args[i]:eval(env))
        end

        return types.toscheme(val(table.unpack(fargs)))
    else
        err(args[1], "undefined lua value: %s", args[1])
    end
end
--]]

special_forms["if"] = function(self, env, args)
    ensure(self, #args == 3,
        "syntax-error",
        "if: insufficient arguments")

    local cond = args[1]:eval(env)

    if is_true(cond) then
        return args[2]:eval(env)
    else
        return args[3]:eval(env)
    end
end

local function wrap_bodies(bodies)
    local types = require "scheme.types"

    if #bodies > 1 then
        return types.list.new{types.ident.new("begin"), table.unpack(bodies)}
    else
        return bodies[1]
    end
end

special_forms["cond"] = function(self, env, args)
    local types = require "scheme.types"

    -- Sanity check before evaluating.
    for i, clause in ipairs(args) do
        ensure(clause, clause:type() == "list",
            "syntax-error",
            "cond-clause must be a list")

        ensure(clause, #clause:getval() >= 2,
            "syntax-error",
            "cond-clause must be a list of at least size 2")

        if clause:getval()[1]:type() == "identifier" and
           clause:getval()[1]:getval() == "else" then

            ensure(clause, i == #args,
                "syntax-error",
                "else-clause must be the last clause in a cond")

            clause:getval()[1] = types.boolean.new(true)
        end
    end

    for i, clause in ipairs(args) do
        local cond = clause:getval()[1]:eval(env)

        if is_true(cond) then
            return wrap_bodies{table.unpack(clause:getval(), 2)}:eval(env)
        end
    end

    return types.list.new{}
end

local function isdot(val)
    return val:type() == "identifier" and val:getval() == "."
end


local quote

local function quote_list(self, env, val, i)
    local types = require "scheme.types"

    if not i then
        -- First time looking at this list, scan to see if there isn't a period
        -- somewhere that is not the last element of the list.
        for j = 1, #val do
            if isdot(val[j]) and j ~= (#val - 1) then
                util.err(val[j],
                    "syntax-error",
                    "malformed pair shortcut form, \".\" must be second " ..
                    "item in list")
            end
        end

        i = 1
    end

    if (#val - i + 1) >= 3 and isdot(val[#val - 1]) then
        -- (x... . y) -> Pair
        local head = quote(self, env, val[i])
        local tail

        if (#val - i + 1) == 3 and isdot(val[i + 1]) then
            -- (x . y) -> complete pair with nothing following, evaluate tail
            tail = quote(self, env, val[i + 2])
        else
            -- (x...  . y) -> grab head and keep evaluating.
            tail = quote_list(self, env, val, i + 1)
        end


        if tail:type() == "list" then
            -- Tail evaluated to list, collapse the whole thing down to a list
            return types.list.new{head, table.unpack(tail:getval())}
        else
            return types.pair.new(head, tail)
        end
    else
        for j = i, #val do
            val[j] = quote(self, env, val[j])
        end

        return types.list.new{table.unpack(val, i)}
    end
end

quote = function(self, env, val)
    if val:type() == "identifier" then
        local v = env:intern(val:getval())
        v:setpos(val:getpos())

        return v
    elseif val:type() == "list" then
        return quote_list(self, env, val:getval())
    else
        return val
    end
end

special_forms["quote"] = function(self, env, args)
    expect_argc(self, 1, #args)

    return quote(self, env, args[1])
end

local function parse_paramslist(paramslist)
    local funcparams = {}
    local varparam   = nil

    local defined_params = {}

    for i, param in ipairs(paramslist) do
        ensure(param, param:type() == "identifier",
            "syntax-error",
            "procedure parameter must be an identifier")

        local paramname = param:getval()

        if paramname == "." then
            if i == #paramslist then
                err(param,
                    "syntax-error",
                    "expected variadic parameter following")

            elseif i ~= #paramslist - 1 then
                err(paramslist[i + 1],
                    "syntax-error",
                    "variadic parameter must be last")
            end

            ensure(paramslist[i + 1],
                paramslist[i + 1]:type() == "identifier",
                "syntax-error",
                "variadic parameter must be an identifier")

            local varpar = paramslist[i + 1]

            ensure(varpar, defined_params[varpar:getval()] == nil,
                "syntax-error",
                "parameter with the same name already exists: %s",
                    varpar:getval())

            defined_params[varpar:getval()] = true

            varparam = varpar:getval()
            break
        else
            ensure(param, defined_params[paramname] == nil,
                "syntax-error",
                "parameter with the same name already exists: %s",
                    paramname)

            defined_params[paramname] = true

            table.insert(funcparams, paramname)
        end
    end

    return funcparams, varparam
end

local function create_lambda(defp, env, funcname, funcparams, varparam, body)
    local types = require "scheme.types"
    local func  = types.proc.new(funcname, env, funcparams, varparam, body)

    func:setpos(defp:getpos())

    return func
end

special_forms["define"] = function(self, env, args)
    ensure(self, #args >= 2,
        "syntax-error",
        "define: insufficient arguments")

    local var, val = table.unpack(args)

    ensure(var,
        var:type() == "identifier" or var:type() == "list",
        "syntax-error",
        "definition must be an identifier or a list")

    -- Are we defining a variable or a procedure?
    if var:type() == "identifier" then
        -- variable!
        ensure(self, #args == 2, "syntax-error")
        ensure(self, not env:is_defined(var:getval()),
            "syntax-error",
            "variable or procedure already defined: %s",
                var:getval())

        env:define(var:getval(), val:eval(env))

    else
        -- procedure!
        ensure(var:getval()[1], var:getval()[1]:type() == "identifier",
            "syntax-error",
            "procedure name must be an identifier")

        local funcname = var:getval()[1]:getval()
        local paramslist = {table.unpack(var:getval(), 2)}

        ensure(self, not env:is_defined(funcname),
            "syntax-error",
            "variable or procedure already defined: %s",
                funcname)

        local funcparams, varparam = parse_paramslist(paramslist)

        val = wrap_bodies{table.unpack(args, 2)}

        env:define(funcname,
            create_lambda(self, env, funcname, funcparams, varparam, val))
    end
end

special_forms["lambda"] = function(self, env, args)
    ensure(self, #args >= 2,
        "syntax-error",
        "lambda: insufficient arguments")

    local paramslist = args[1]

    ensure(paramslist, paramslist:type() == "list",
        "syntax-error",
        "parameter list must be a list")

    local funcparams, varparam = parse_paramslist(paramslist:getval())
    return create_lambda(self, env, nil, funcparams, varparam,
        wrap_bodies{table.unpack(args, 2)})
end

special_forms["begin"] = function(self, env, args)
    for i, e in ipairs(args) do
        if i == #args then
            return e:eval(env)
        else
            e:eval(env)
        end
    end
end


local function parse_bindings(bindings)
    local params = {}
    local args   = {}

    local defined = {}

    ensure(bindings, bindings:type() == "list",
        "syntax-error",
        "bindings list must be a list")

    for i, binding in ipairs(bindings:getval()) do
        ensure(binding, binding:type() == "list",
            "syntax-error",
            "binding must be a list")

        ensure(binding, #binding:getval() == 2,
            "syntax-error",
            "binding must be a list of size 2")

        ensure(binding, binding:getval()[1]:type() =="identifier",
            "syntax-error",
            "binding name must be an identifier")

        local bindvar, bindval = table.unpack(binding:getval())

        ensure(bindvar, defined[bindvar:getval()] == nil,
            "syntax-error",
            "binding with the same name already exists: %s",
                bindvar:getval())

        defined[bindvar:getval()] = true

        table.insert(params, bindvar:getval())
        table.insert(args,   bindval)
    end

    return params, args
end

special_forms["let"] = function(self, env, args)
    --  (let ((a a-val) (b b-val)) body)
    --      -> ((lambda (a b) body) a-val b-val)
    --
    ensure(self, #args >= 2,
        "syntax-error",
        "let: insufficient arguments")

    local bindings = args[1]
    local params, largs = parse_bindings(bindings)

    local letenv = env:derive("let")

    for i = 1, #params do
        letenv:define(params[i], largs[i]:eval(env))
    end

    return wrap_bodies{table.unpack(args, 2)}:eval(letenv)
end

special_forms["let*"] = function(self, env, args)
    --  (let* ((a a-val) (b b-val)) body)
    --      -> ((lambda (a)
    --            ((lambda (b)
    --               body)
    --             b-val))
    --          a-val)
    --
    ensure(self, #args >= 2,
        "syntax-error",
        "let*: insufficient arguments")

    local bindings = args[1]
    local params, largs = parse_bindings(bindings)
    local letenv = env

    for i = 1, #params do
        local val = largs[i]:eval(letenv)

        letenv = letenv:derive("let*-" .. params[i])
        letenv:define(params[i], val)
    end

    return wrap_bodies{table.unpack(args, 2)}:eval(letenv)
end

special_forms["letrec"] = function(self, env, args)
    -- TODO
    util.not_implemented(self)
end

return special_forms
