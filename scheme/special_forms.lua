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

    util.expect(cond, "boolean")
    if cond:getval() then
        return args[2]:eval(env)
    else
        return args[3]:eval(env)
    end
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

            if defined_params[varpar:getval()] then
                util.err(varpar,
                    "parameter with the same name already exists: %s",
                    varpar:getval())
            else
                defined_params[varpar:getval()] = true
            end

            varparam = varpar:getval()
            break
        else
            if defined_params[paramname] then
                util.err(param,
                    "parameter with the same name already exists: %s",
                    paramname)
            else
                defined_params[paramname] = true
            end

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
    util.expect_argc(self, 2, #args)

    local var, val = table.unpack(args)

    util.expect(var, {"atom",  "list"}, "definition")

    -- Are we defining a variable or a function?
    if var:type() == "atom" then
        -- variable!
        if env:is_defined(var:getval()) then
            util.err(self, "variable or function already defined: %s",
                var:getval())
        end

        env:define(var:getval(), val:eval(env))

    else
        -- function!
        util.expect(var:car(), "atom", "function name")

        local funcname = var:car():getval()
        local paramslist = var:cdr()

        if env:is_defined(funcname) then
            util.err(self, "variable or function already defined: %s",
                funcname)
        end

        local funcparams, varparam = parse_paramslist(paramslist)

        env:define(funcname,
            create_lambda(self, env, funcname, funcparams, varparam, val))
    end
end

special_forms["lambda"] = function(self, env, args)
    util.expect_argc(self, 2, #args)

    local paramslist, body = table.unpack(args)

    util.expect(paramslist, "list")

    local funcparams, varparam = parse_paramslist(paramslist:getval())
    return create_lambda(self, env, "lambda", funcparams, varparam, body)
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

        if #binding:getval() ~= 2 then
            util.err(binding, "binding must be a list of size 2")
        end

        util.expect(binding:getval()[1], "atom")

        local bindvar, bindval = table.unpack(binding:getval())

        if defined[bindvar:getval()] then
            util.err(bindvar, "binding with the same name already exists: %s",
                bindvar:getval())
        else
            defined[bindvar:getval()] = true
        end

        table.insert(params, bindvar:getval())
        table.insert(args,   bindval)
    end

    return params, args
end

special_forms["let"] = function(self, env, args)
    --  (let ((a a-val) (b b-val)) body)
    --      -> ((lambda (a b) body) a-val b-val)
    --
    util.expect_argc(self, 2, #args)

    local bindings, body = table.unpack(args)
    local params, args = parse_bindings(bindings)

    local letenv = env:derive("let")

    for i = 1, #params do
        letenv:define(params[i], args[i]:eval(env))
    end

    return body:eval(letenv)
end

special_forms["let*"] = function(self, env, args)
    --  (let* ((a a-val) (b b-val)) body)
    --      -> ((lambda (a)
    --            ((lambda (b)
    --               body)
    --             b-val))
    --          a-val)
    --
    util.expect_argc(self, 2, #args)

    local bindings, body = table.unpack(args)

    util.expect(bindings, "list")

    local params, args = parse_bindings(bindings)
    local letenv = env

    for i = 1, #params do
        local val = args[i]:eval(letenv)

        letenv = letenv:derive("let*-" .. params[i])
        letenv:define(params[i], val)
    end

    return body:eval(letenv)
end

special_forms["letrec"] = function(self, env, args)
    -- TODO
    util.not_implemented(self)
end

return special_forms
