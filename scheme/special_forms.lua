local special_forms = {}

local util = require "scheme.util"

local err = util.err
local ensure = util.ensure
local expect = util.expect
local expect_argc = util.expect_argc
local expect_argc_min = util.expect_argc_min
local expect_argc_max = util.expect_argc_max
local is_true = util.is_true

local unpack = table.unpack or unpack

--[[
-- Helpers
--]]
local function wrap_and_swap(bodies, offset)
    local types = require "scheme.types"

    if (#bodies - offset + 1) > 1 then
        local repl = types.list.new{
            types.ident.new("begin"),
            unpack(bodies, offset)}

        for i = offset, #bodies - 1 do
            table.remove(bodies)
        end

        bodies[#bodies] = repl
    end
end

local function maybe_eval(val, env)
    if val.self_eval then
        return val
    else
        return val:eval(env)
    end
end


special_forms.__pre = {}

--[[
-- if-statement
--
-- (if <condition> <true-branch> <false-branch>)
--]]
special_forms.__pre["if"] = function(def, env)
    ensure(def[1], #def == 4,
        "syntax-error",
        "malformed if-statement: expected: " ..
        "(if <condition> <true-branch> <false-branch>)")

    def[2]:preprocess(env)
    def[3]:preprocess(env)
    def[4]:preprocess(env)
end

special_forms["if"] = function(self, env, args)
    if is_true(maybe_eval(args[1], env)) then
        return maybe_eval(args[2], env)
    else
        return maybe_eval(args[3], env)
    end
end

--[[
-- cond-statement
--
-- (cond (<clause|else> <body...>...))
--]]
special_forms.__pre["cond"] = function(def, env)
    local types = require "scheme.types"

    for i = 2, #def do
        local clause = def[i]

        ensure(clause, clause.type == "list",
            "syntax-error",
            "cond-clause must be a list, not value of type %s: %s",
                clause.type,
                tostring(clause))

        ensure(clause, #clause:getval() >= 2,
            "syntax-error",
            "cond-clause must be a list of at least size 2")

        local cond = clause:getval()[1]

        if cond.type == "identifier" and cond:getval() == "else" then
            ensure(cond, i == #def,
                "syntax-error",
                "else-clause must be the last cond-clause")

            clause:getval()[1] = types.boolean.new(true)
        end

        wrap_and_swap(clause:getval(), 2)

        clause:getval()[2]:preprocess(env)
    end
end

special_forms["cond"] = function(self, env, args)
    local types = require "scheme.types"

    for i, clause in ipairs(args) do
        if is_true(maybe_eval(clause:getval()[1], env)) then
            return maybe_eval(clause:getval()[2], env)
        end
    end

    return types.list.new{}
end

--[[
-- quote function
--
-- (quote <val>)
--]]
local quote

local function isdot(val)
    return val.type == "identifier" and val:getval() == "."
end

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


        if tail.type == "list" then
            -- Tail evaluated to list, collapse the whole thing down to a list
            return types.list.new{head, unpack(tail:getval())}
        else
            return types.pair.new(head, tail)
        end
    else
        for j = i, #val do
            val[j] = quote(self, env, val[j])
        end

        return types.list.new{unpack(val, i)}
    end
end

function quote(self, env, val)
    if val.type == "identifier" then
        local v = env:intern(val:getval())
        v:setpos(val:getpos())

        return v
    elseif val.type == "list" then
        return quote_list(self, env, val:getval())
    else
        return val
    end
end


special_forms.__pre["quote"] = function(def, env)
    ensure(def[1], #def == 2,
        "syntax-error",
        "malformed quote: expected: (quote <val>)")
end

special_forms["quote"] = function(self, env, args)
    return quote(self, env, args[1])
end

--[[
-- proc helpers
--]]
local function process_paramslist(env, funcname, params, offset)
    local defined = {}
    local funcparams = {}
    local varparam = nil

    for i = offset, #params do
        local param = params[i]

        ensure(param, param.type == "identifier",
            "syntax-error",
            "procedure parameter must be an identifier, " ..
            "not value of type %s: %s",
                param.type,
                tostring(param))

        local paramname = param:getval():lower()

        if paramname == "." then
            if i == #params then
                err(param, "syntax-error", "expected variadic parameter")
            elseif i ~= #params - 1 then
                err(params[i + 1],
                    "syntax-error",
                    "variadic parameter must be last")
            end

            varparam = params[i + 1]

            ensure(varparam, varparam.type == "identifier",
                "syntax-error",
                "variadic parameter must be an identifier, " ..
                "not value of type %s: %s",
                    varparam.type,
                    tostring(varparam))

            local varparamname = varparam:getval():lower()

            if funcname then
                ensure(varparam, varparamname ~= funcname,
                    "syntax-error",
                    "parameter name may not be the same as procedure name")
            end

            ensure(varparam, not defined[varparamname],
                "syntax-error",
                "parameter with the same name already exists: %s",
                    varparam:getval())

            defined[varparamname] = true
            varparam = varparamname

            break
        else
            if funcname then
                ensure(param, paramname ~= funcname,
                    "syntax-error",
                    "parameter name may not be the same as procedure name")
            end

            ensure(param, not defined[paramname],
                "syntax-error",
                "parameter with the same name already exists: %s",
                    param:getval())

            defined[paramname] = true

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


--[[
-- define-statement
--
-- (define <val> <body>) | (define (<proc> [param...]) <body...>)
--]]
special_forms.__pre["define"] = function(def, env)
    local self, var = def[1], def[2]

    ensure(self, #def >= 3,
        "syntax-error",
        "malformed define: expected: (define <val> <var>) or " ..
                                    "(define (<name> [param...]) <body...>)")

    ensure(var, var.type == "identifier" or var.type == "list",
        "syntax-error",
        "definition must be an identifier or a list, not value of type %s: %s",
            var.type,
            tostring(var))

    if var.type == "identifier" then
        ensure(self, #def:getval() == 3,
            "syntax-error",
            "malformed definition: expected (define <ident> <val>)")
    else
        local head = var:getval()[1]

        ensure(head, head.type == "identifier",
            "syntax-error",
            "procedure name must be an identifier, not value of type %s: %s",
                head.type,
                tostring(head))

        ensure(head, head:getval() ~= ".",
            "syntax-error",
            "invalid procedure name: .")

        self.params, self.varparam = process_paramslist(
            env,
            head:getval():lower(),
            var:getval(),
            2)

        wrap_and_swap(def, 3)
    end

    def[3]:preprocess(env)
end

special_forms["define"] = function(self, env, args)
    local var, val = args[1], args[2]

    -- Are we defining a variable or a procedure?
    if var.type == "identifier" then
        -- variable!
        return env:define(var:getval(), maybe_eval(val, env))

    else
        -- procedure!
        local head = var:getval()[1]
        local funcname = head:getval()

        return env:define(funcname,
            create_lambda(
                head,
                env,
                funcname,
                self.params,
                self.varparam,
                args[2]))
    end
end

--[[
-- set! function
--
-- (set! <var> <new-val>)
--]]
special_forms.__pre["set!"] = function(def, env)
    ensure(def[1], #def == 3,
        "syntax-error",
        "malformed set!: expected (set! <var> <val>)")

    ensure(def[2], def[2].type == "identifier",
        "syntax-error",
        "definition must be an identifier, not a value of type %s: %s",
            def[2].type,
            tostring(def[2]))

    def[3]:preprocess(env)
end

special_forms["set!"] = function(self, env, args)
    return env:define(args[1]:getval(), maybe_eval(args[2], env))
end

--[[
-- lambda-statement
--
-- (lambda ([param...]) <body...>)
--]]
special_forms.__pre["lambda"] = function(def, env)
    local self, params = def[1], def[2]

    ensure(self, #def >= 3,
        "syntax-error",
        "malformed lambda-statement: expected: (lambda ([param...]) <body...>)")

    ensure(params, params.type == "list",
        "syntax-error",
        "parameter list must be a list, not value of type %s: %s",
            params.type,
            tostring(params))

    self.params, self.varparam =
        process_paramslist(env, nil, params:getval(), 1)

    wrap_and_swap(def, 3)

    def[3]:preprocess(env)
end

special_forms["lambda"] = function(self, env, args)
    return create_lambda(self, env, nil, self.params, self.varparam, args[2])
end

--[[
-- begin-statement
--
-- (begin <body...>)
--]]
special_forms.__pre["begin"] = function(def, env)
    ensure(def[1], #def > 1,
        "syntax-error",
        "malformed begin: expected: (begin <body...>)")
end

special_forms["begin"] = function(self, env, args)
    for i, e in ipairs(args) do
        if i == #args then
            return maybe_eval(e, env)
        else
            maybe_eval(e, env)
        end
    end
end

--[[
-- let/let* helper
--]]
local function process_bindings(env, bindings)
    local params = {}
    local args   = {}

    local defined = {}

    ensure(bindings, bindings.type == "list",
        "syntax-error",
        "binding list must be a list, not value of type %s: %s",
            bindings.type,
            tostring(bindings))

    for i, binding in ipairs(bindings:getval()) do
        ensure(binding, binding.type == "list",
            "syntax-error",
            "binding must be a list, not value of type %s: %s",
                binding.type,
                tostring(binding))

        ensure(binding, #binding:getval() == 2,
            "syntax-error",
            "binding must be a list of size 2")

        local bindvar, bindval = binding:getval()[1], binding:getval()[2]

        ensure(bindvar, bindvar.type == "identifier",
            "syntax-error",
            "binding name must be an identifier, not value of type %s: %s",
                bindvar.type,
                tostring(bindvar))

        local bindvarname = bindvar:getval():lower()

        ensure(bindvar, not defined[bindvarname],
            "syntax-error",
            "binding with the same name already exists: %s",
                bindvar:getval())

        defined[bindvarname] = true

        bindval:preprocess(env)

        table.insert(params, bindvarname)
        table.insert(args, bindval)
    end

    return params, args
end

--[[
-- let-statement
--
-- (let ((<var> <binding>)...) <body...)
--]]
special_forms.__pre["let"] = function(def, env)
    local self, bind = def[1], def[2]

    ensure(self, #def >= 3,
        "syntax-error",
        "malformed let-statement: expected: (let (<binding...>) <body...>)")

    self.bindvars, self.bindings = process_bindings(env, bind)
    wrap_and_swap(def, 3)

    def[3]:preprocess(env)
end

special_forms["let"] = function(self, env, args)
    --  (let ((a a-val) (b b-val)) body)
    --      -> ((lambda (a b) body) a-val b-val)
    --
    local letenv = env:derive("let")

    for i = 1, #self.bindvars do
        letenv:define(self.bindvars[i], maybe_eval(self.bindings[i], env))
    end

    return maybe_eval(args[2], letenv)
end

--[[
-- let*-statement
--
-- (let* ((<var> <binding>)...) <body...)
--]]
special_forms.__pre["let*"] = function(def, env)
    local self, bind = def[1], def[2]

    ensure(self, #def >= 3,
        "syntax-error",
        "malformed let*-statement: expected: (let* (<binding...>) <body...>)")

    self.bindvars, self.bindings = process_bindings(env, bind)
    wrap_and_swap(def, 3)

    def[3]:preprocess(env)
end

special_forms["let*"] = function(self, env, args)
    --  (let* ((a a-val) (b b-val)) body)
    --      -> ((lambda (a)
    --            ((lambda (b)
    --               body)
    --             b-val))
    --          a-val)
    --
    local letenv = env

    for i = 1, #self.bindvars do
        local val = maybe_eval(self.bindings[i], letenv)

        letenv = letenv:derive("let*-" .. self.bindvars[i])
        letenv:define(self.bindvars[i], val)
    end

    return maybe_eval(args[2], letenv)
end

return special_forms
