local special_forms = {}

local util = require "scheme.util"

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

local function parse_argslist(argslist)
    local funcargs = {}
    local vararg   = nil

    for i, arg in ipairs(argslist) do
        util.expect(arg, "atom", "function parameter")

        local argname = arg:getval()

        if argname == "." then
            if i == #argslist then
                util.err(arg, "expected variadic parameter following")
            elseif i ~= #argslist - 1 then
                util.err(argslist[i + 1], "variadic parameter must be last")
            end

            util.expect(argslist[i + 1], "atom", "variadic parameter")
            vararg = argslist[i + 1]:getval()
            break
        else
            table.insert(funcargs, argname)
        end
    end

    return funcargs, vararg
end

special_forms["define"] = function(self, env, args)
    util.expect_argc(self, 2, #args)

    local var, val = table.unpack(args)

    util.expect(var, {"atom",  "list"}, "definition")

    -- Are we defining a variable or a function?
    if var:type() == "atom" then
        -- variable!
        env:define(var:getval(), val:eval(env))

    else
        -- function!
        util.expect(var:car(), "atom", "function name")

        local funcname = var:car():getval()
        local argslist = var:cdr()
        local funcargs, vararg = parse_argslist(argslist)

        local types = require "scheme.types"
        local func  = types.mkfunction(env:derive(funcname), funcargs, val)

        func.name   = funcname
        func.vararg = vararg
        func:setpos(self:getpos())

        env:define(funcname, func)
    end
end

special_forms["lambda"] = function(self, env, args)
    util.expect_argc(self, 2, #args)

    local argslist, body = table.unpack(args)

    util.expect(argslist, "list")

    local funcargs, vararg = parse_argslist(argslist:getval())

    local types = require "scheme.types"
    local func = types.mkfunction(env:derive("lambda"), funcargs, body)

    func.vararg = vararg
    func:setpos(self:getpos())

    return func
end

special_forms["do"] = function(self, env, args)
    local last

    for i, e in ipairs(args) do
        last = e:eval(env)
    end

    return last
end


local function parse_bindings(bindings)
    local params = {}
    local args   = {}

    util.expect(bindings, "list")

    for i, binding in ipairs(bindings:getval()) do
        util.expect(binding, "list")
        util.expect(binding:getval()[1], "atom")

        table.insert(params, binding:getval()[1])
        table.insert(args,   binding:getval()[2])
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

    local types = require "scheme.types"

    local lam = {
        special_forms["lambda"](self, env, { types.mklist(params), body})
    }

    for i, arg in ipairs(args) do
        table.insert(lam, arg)
    end

    return types.mklist(lam):eval(env)
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

    local types = require "scheme.types"
    local params, args = parse_bindings(bindings)

    local function mklambdas(i)
        return types.mklist{
            special_forms["lambda"](self, env, {
                types.mklist{params[#params - i + 1]},
                i == 1 and body or mklambdas(i - 1)}),
            args[#args - i + 1]}
    end

    return mklambdas(#params):eval(env)
end

special_forms["letrec"] = function(self, env, args)
    -- TODO
    util.not_implemented(self)
end

return special_forms
