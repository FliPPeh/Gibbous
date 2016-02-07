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

local function valid_ident(val)
    return val.type == "identifier" and val:getval():sub(1, 1):find("%w")
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
-- quote helpers
--]]
local quote

local function is_call(val, fn)
    return val.type == "list" and
        #val:getval() >= 1 and
        val:getval()[1].type == "identifier" and
        val:getval()[1]:getval() == fn
end

local function is_unquote(val)
    return is_call(val, "unquote")
end

local function is_unquote_splicing(val)
    return is_call(val, "unquote-splicing")
end

local function is_quasiquote(val)
    return is_call(val, "quasiquote") or is_call(val, "quasisyntax")
end


local function quote_list(self, env, val, is_quasi, is_syntax, lvl)
    local types = require "scheme.types"
    local j = 1

    while j <= #val do
        if is_quasi and is_unquote_splicing(val[j]) then
            if lvl == 0 then
                ensure(val[j], #val[j]:getval() == 2,
                    "syntax-error",
                    "malformed unquote-splicing: " ..
                    "expected: (unquote-splicing <val>)")

                local res = val[j]:getval()[2]:eval(env)

                ensure(res, res.type == "list",
                    "type-error",
                    "unquote-splicing must evaluate to a list, " ..
                    "not value of type %s: %s",
                        res.type,
                        tostring(res))

                table.remove(val, j)

                for i = #res:getval(), 1, -1 do
                    table.insert(val, j, res:getval()[i])
                end

                j = j + #res:getval() - 1
            else
                val[j] = quote(self, env, val[j], is_quasi, is_syntax, lvl - 1)
            end

        elseif is_quasi and is_unquote(val[j]) then
            if lvl == 0 then
                ensure(val[j], #val[j]:getval() == 2,
                    "syntax-error",
                    "malformed unquote: expected: (unquote <val>)")

                val[j] = val[j]:getval()[2]:eval(env)
            else
                val[j] = quote(self, env, val[j], is_quasi, is_syntax, lvl - 1)
            end

        elseif is_quasiquote(val[j]) then
            val[j] = quote(self, env, val[j], is_quasi, is_syntax, lvl + 1)
        else
            val[j] = quote(self, env, val[j], is_quasi, is_syntax, lvl)
        end

        j = j + 1
    end

    return types.list.new{unpack(val, i)}
end

function quote(self, env, val, is_quasi, is_syntax, lvl)
    local types = require "scheme.types"

    if val.type == "identifier" then
        local v

        if not is_syntax and val:getval() ~= "." then
            v = env:intern(val:getval())
        else
            if val:getval() ~= "." then
                -- TODO: Mark identifier for {bound,free,literal}-identifier=?
                v = types.ident.new(val:getval())
            else
                v = val
            end
        end

        v:setpos(val:getpos())

        return v
    elseif val.type == "list" then
        -- Special case, only do this on the first pass before we get a chance
        -- to recurse back here from quote_list.
        if is_unquote(val) and not lvl then
            return val:getval()[2]:eval(env)
        end

        lvl = lvl or 0
        return quote_list(self, env, val:getval(), is_quasi, is_syntax, lvl)
    else
        return val
    end
end


local function isdot(val)
    return (val.type == "identifier" or val.type == "symbol")
        and val:getval() == "."
end

local quote_helper

local function build_pair(val, i)
    local types = require "scheme.types"

    local head = val[i]
    local tail

    if (#val - i + 1) == 3 and isdot(val[i + 1]) then
        -- (x . y) -> complete pair with nothing following
        tail = val[i + 2]
    else
        -- (x...  . y) -> grab head and keep evaluating.
        tail = build_pair(val, i + (isdot(val[i + 1]) and 2 or 1))
    end

    head = quote_helper(head)
    tail = quote_helper(tail)

    if tail.type == "list" then
        -- Tail evaluated to list, collapse the whole thing down to a list
        return types.list.new{head, unpack(tail:getval())}
    else
        return types.pair.new(head, tail)
    end
end

function quote_helper(val)
    if val.type == "list" then
        local is_pair = false
        local vall = val:getval()

        for i, v in ipairs(vall) do
            if isdot(v) then
                --[[
                -- TODO: Be less strict about stray periods and keep invalid
                --       pair forms as lists?
                --]]
                ensure(v, i == (#vall - 1),
                    "syntax-error",
                    "malformed pair shortcut form, \".\" must be second " ..
                    "last item in list")

                is_pair = true
            end

            vall[i] = quote_helper(v)
        end

        if is_pair then
            return build_pair(val, 1)
        end
    end

    return val
end

--[[
-- quote function
--
-- (quote <val>)
--]]
special_forms.__pre["quote"] = function(def, env)
    ensure(def[1], #def == 2,
        "syntax-error",
        "malformed quote: expected: (quote <val>)")
end

special_forms["quote"] = function(self, env, args)
    return quote_helper(quote(self, env, args[1], false, false))
end

--[[
-- quasiquote
--
-- (quasiquote <val>)
--]]
local function process_quasiquote(env, var)
    if is_unquote(var) or is_unquote_splicing(var) then
        var[2]:preprocess(env)

    elseif var.type == "list" then
        for _, v in ipairs(var) do
            process_quasiquote(env, v)
        end
    end
end

special_forms.__pre["quasiquote"] = function(def, env)
    ensure(def[1], #def == 2,
        "syntax-error",
        "malformed quasiquote: expected: (quasiquote <val>)")

    process_quasiquote(env, def[2])
end

special_forms["quasiquote"] = function(self, env, args)
    return quote_helper(quote(self, env, args[1], true, false))
end

--[[
-- syntax function
--
-- (syntax <val>)
--]]
special_forms.__pre["syntax"] = function(def, env)
    ensure(def[1], #def == 2,
        "syntax-error",
        "malformed syntax: expected: (syntax <val>)")
end

special_forms["syntax"] = function(self, env, args)
    -- TODO
    return quote_helper(quote(self, env, args[1], false, true))
end

--[[
-- quasisyntax
--
-- (quasisyntax <val>)
--]]
special_forms.__pre["quasisyntax"] = function(def, env)
    ensure(def[1], #def == 2,
        "syntax-error",
        "malformed quasisyntax: expected: (quasisyntax <val>)")

    process_quasiquote(env, def[2])
end

special_forms["quasisyntax"] = function(self, env, args)
    -- TODO
    return quote_helper(quote(self, env, args[1], true, true))
end

--[[
-- unquote
--
-- (unquote <val>)
--]]
special_forms.__pre["unquote"] = function(def, env)
    ensure(def[1], #def == 2,
        "syntax-error",
        "malformed unquote: expected: (unquote <val>)")
end

special_forms["unquote"] = function(self, env, args)
    err(self, "syntax-error", "unquote outside quote is not valid")
end

--[[
-- unquote-splicing
--
-- (unquote-splicing <val>)
--]]
special_forms.__pre["unquote-splicing"] = function(def, env)
    ensure(def[1], #def == 2,
        "syntax-error",
        "malformed unquote-splicing: expected: (unquote-splicing <val>)")
end

special_forms["unquote-splicing"] = function(self, env, args)
    err(self, "syntax-error", "unquote-splicing outside quote is not valid")
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

            ensure(varparam, valid_ident(varparam),
                "syntax-error",
                "invalid identifier")

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

            ensure(param, valid_ident(param),
                "syntax-error",
                "invalid identifier")

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
        ensure(var, valid_ident(var),
            "syntax-error",
            "invalid identifier")

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

        ensure(head, valid_ident(head),
            "syntax-error",
            "invalid identifier")

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
    local var, val = def[2], def[3]

    ensure(def[1], #def == 3,
        "syntax-error",
        "malformed set!: expected (set! <var> <val>)")

    ensure(var, var.type == "identifier",
        "syntax-error",
        "definition must be an identifier, not value of type %s: %s",
            var.type,
            tostring(var))

    ensure(var, valid_ident(var),
        "syntax-error",
        "invalid identifier")

    val:preprocess(env)
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

    for i = 2, #def do
        def[i]:preprocess(env)
    end
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

        ensure(bindvar, valid_ident(bindvar),
            "syntax-error",
            "invalid identifier")

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
-- (let ((<var> <binding>)...) <body...>)
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


local function package_config(cfg)
    local r = {}

    for k in cfg:gmatch("(.-)\n") do
        table.insert(r, k)
    end

    return unpack(r)
end

local function locate(mod, paths)
    local psep, sep, wild, _, _ = package_config(package.config)
    local sp = paths:gsub("%" .. wild, mod:gsub("%.", psep), nil)

    for m in string.gmatch(sp, "[^%" .. sep .. "]+") do
        local f = io.open(m, "r")
        if f then
            return m
        end
    end

    return nil
end

--[[
-- import-statement
--
-- (import <module>)
--]]
special_forms.__pre["import"] = function(def, env)
    local self, mod, bind = def[1], def[2], def[3]

    ensure(self, #def == 2,
        "syntax-error",
        "malformed import-statement: expected: (import <module>)")

    ensure(mod, mod.type == "string",
        "syntax-error",
        "module import must be a string")
end

special_forms["import"] = function(self, env, args)
    local types = require "scheme.types"

    local module = args[1]:getval()
    local bindvar = select(3, args[1]:getval():find("([^.]+)$"))

    if not env.imported[module] then
        local mod, errmsg = package.searchpath(module, env.search_paths)

        if mod then
            env.imported[module] = env:eval_file(mod)
		else
			err(args[1], "import-error", "module '%s' not found:%s",
				module,
				errmsg)
        end
    end

    return types.toscheme(env.imported[module])
end

--[[
-- catch-statement
--
-- (catch <body...>)
--]]
special_forms.__pre["catch"] = function(def, env)
    wrap_and_swap(def, 2)

    def[2]:preprocess(env)
end

special_forms["catch"] = function(self, env, args)
    local types = require "scheme.types"
    local res = {pcall(function() return args[1]:eval(env) end)}

    return types.pair.new(types.boolean.new(res[1]), res[2])
end

return special_forms
