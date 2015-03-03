local special_forms = {}

local util = require "scheme.util"

--[[
special_forms["."] = function(self, env, args)
    local types = require "scheme.types"

    util.expect_argc_min(self, 1, #args)

    local head = args[1]

    while head:type() ~= "atom" do
        head = head:eval(env)
    end

    local val = env:locate_lua(head:getval())

    if val ~= nil and type(val) ~= "function" then
        util.expect_argc_max(self, 1, #args)

        return types.toscheme(val)
    elseif val ~= nil then
        local fargs = {}

        for i = 2, #args do
            fargs[i - 1] = types.tolua(args[i]:eval(env))
        end

        return types.toscheme(val(table.unpack(fargs)))
    else
        util.err(args[1], "undefined lua value: %s", args[1])
    end
end
--]]

special_forms["if"] = function(self, env, args)
    util.expect_argc(self, 3, #args)

    local cond = args[1]:eval(env)

    if cond:type() ~= "boolean" or cond:getval() then
        return args[2]:eval(env)
    else
        return args[3]:eval(env)
    end
end

local function wrap_bodies(bodies)
    local types = require "scheme.types"

    if #bodies > 1 then
        return types.list.new{types.atom.new("begin"), table.unpack(bodies)}
    else
        return bodies[1]
    end
end

special_forms["cond"] = function(self, env, args)
    local types = require "scheme.types"

    -- Sanity check before evaluating.
    for i, clause in ipairs(args) do
        util.expect(clause, "list")
        util.ensure(clause, #clause:getval() >= 2,
            "cond-clause must be a list of at least size 2")

        if clause:getval()[1]:type() == "atom" and
           clause:getval()[1]:getval() == "else" then

           util.ensure(clause, i == #args,
               "else-clause must be the last clause in a cond")

           clause:getval()[1] = types.boolean.new(true)
       end
    end

    for i, clause in ipairs(args) do
        local cond = clause:getval()[1]:eval(env)

        if cond:type() ~= "boolean" or cond:getval() then
            return wrap_bodies{table.unpack(clause:getval(), 2)}:eval(env)
        end
    end

    return types.list.new{}
end

special_forms["quote"] = function(self, env, args)
    util.expect_argc(self, 1, #args)

    return args[1]
end

local function parse_paramslist(paramslist)
    local funcparams = {}
    local varparam   = nil

    local defined_params = {}

    for i, param in ipairs(paramslist) do
        util.expect(param, "atom", "function parameter")

        local paramname = param:getval()

        if paramname == "." then
            if i == #paramslist then
                util.err(param, "expected variadic parameter following")
            elseif i ~= #paramslist - 1 then
                util.err(paramslist[i + 1], "variadic parameter must be last")
            end

            util.expect(paramslist[i + 1], "atom", "variadic parameter")

            local varpar = paramslist[i + 1]

            util.ensure(varpar, defined_params[varpar:getval()] == nil,
                "parameter with the same name already exists: %s",
                    varpar:getval())

            defined_params[varpar:getval()] = true

            varparam = varpar:getval()
            break
        else
            util.ensure(param, defined_params[paramname] == nil,
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
    local func  = types.func.new(funcname, env, funcparams, varparam, body)

    func:setpos(defp:getpos())

    return func
end

special_forms["define"] = function(self, env, args)
    util.expect_argc_min(self, 2, #args)

    local var, val = table.unpack(args)

    util.expect(var, {"atom",  "list"}, "definition")

    -- Are we defining a variable or a function?
    if var:type() == "atom" then
        -- variable!
        util.expect_argc_max(self, 2, #args)

        util.ensure(self, not env:is_defined(var:getval()),
            "variable or function already defined: %s",
                var:getval())

        env:define(var:getval(), val:eval(env))

    else
        -- function!
        util.expect(var:car(), "atom", "function name")

        local funcname = var:car():getval()
        local paramslist = var:cdr()

        util.ensure(self, not env:is_defined(funcname),
            "variable or function already defined: %s",
                funcname)

        local funcparams, varparam = parse_paramslist(paramslist)

        val = wrap_bodies{table.unpack(args, 2)}

        env:define(funcname,
            create_lambda(self, env, funcname, funcparams, varparam, val))
    end
end

special_forms["lambda"] = function(self, env, args)
    util.expect_argc_min(self, 2, #args)

    local paramslist = args[1]

    util.expect(paramslist, "list")

    local funcparams, varparam = parse_paramslist(paramslist:getval())
    return create_lambda(self, env, "lambda", funcparams, varparam,
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

    util.expect(bindings, "list")

    for i, binding in ipairs(bindings:getval()) do
        util.expect(binding, "list")
        util.ensure(binding, #binding:getval() == 2,
            "binding must be a list of size 2")

        util.expect(binding:getval()[1], "atom")

        local bindvar, bindval = table.unpack(binding:getval())

        util.ensure(bindvar, defined[bindvar:getval()] == nil,
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
    util.expect_argc_min(self, 2, #args)

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
    util.expect_argc_min(self, 2, #args)

    local bindings = args[1]

    util.expect(bindings, "list")

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
