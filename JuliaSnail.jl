import Pkg


module JuliaSnail


import Printf
import Sockets


export start, stop


### --- Elisp s-expression constructor

struct ElispKeyword
   kw::Symbol
end

"""
Construct Elisp expression.

julia> elexpr((:mapcar, (:function, :exp), [1, 2, 3]))
"(mapcar (function exp) '(1 2 3))"

This result can be fed into (eval (read ...)) in Elisp.
"""
function elexpr(arg::Tuple)
   Printf.@sprintf("(%s)", join(map(elexpr, arg), " "))
end

function elexpr(arg::Array)
   Printf.@sprintf("'(%s)", join(map(elexpr, arg), " "))
end

function elexpr(arg::String)
   Printf.@sprintf("\"%s\"", escape_string(arg))
end

function elexpr(arg::Number)
   arg
end

function elexpr(arg::Symbol)
   string(arg)
end

function elexpr(arg::Any)
   "nil"
end

function elexpr(arg::ElispKeyword)
   Printf.@sprintf(":%s", arg.kw)
end


### --- evaluator for running Julia code in a given module

"""
Call eval on expr in the context of the module given by the
fully_qualified_module_name, which has the form of an array of symbols. If the
first element of the fully-qualified module name is implicitly :Main, then it
can be omitted.

```julia
eval_in_module([:One, :Two, :Three], :(x = 3 + 5))
```
is equivalent to
```julia
Main.One.Two.Three.eval(:(x = 3 + 5))
```
"""
function eval_in_module(fully_qualified_module_name::Array{Symbol, 1}, expr::Expr)
   # Retrieving the first module in the chain can be tricky. In general, using
   # getfield to find a module works, but packages loaded as transitive
   # dependencies are not necessarily loaded into Main, and so must be found
   # using another mechanism, i.e., the Base.root_module trick.
   # https://discourse.julialang.org/t/resolving-a-module-by-its-name/30569/6
   root = first(fully_qualified_module_name)
   fqm = try
      getfield(Main, root)
   catch err
      if isa(err, UndefVarError)
         Base.root_module(Base.__toplevel__, root)
      else
         rethrow(err)
      end
   end
   for m in fully_qualified_module_name[2:end]
      fqm = getfield(fqm, m)
   end
   Core.eval(fqm, expr)
end


### --- generic helpers

"""
try-catch wrapper.
"""
macro ignoreerr(expr, retval)
   return quote
      try
         $(esc(expr))
      catch
         $retval
      end
   end
end


### --- introspection helpers

"""
...
"""

function lsnames(ns; all=false, imported=false, include_modules=false, recursive=false, prepend_ns=false, first_call=true)
   raw = names(ns, all=all, imported=imported)
   # remove names containing '#' since Elisp doesn't like them
   raw_clean = filter(
      n -> !occursin(r"#", string(n)),
      raw)
   # remove :eval and :include since they are automatically everywhere
   raw_clean = filter(
      n -> n ∉ (:eval, :include),
      raw_clean)
   # if ns is Main, remove problematic entries
   if ns == Main
      raw_clean = filter(
         n -> n ∉ (:Base, :Core, :InteractiveUtils, :Pkg, :ans),
         raw_clean)
   end
   # remove self-matches
   raw_clean = filter(
      n -> @ignoreerr(n == :missing || Core.eval(ns, n) != ns, false),
      raw_clean)
   # separate out output by module and non-module
   all = filter(
      n -> @ignoreerr(typeof(Core.eval(ns, n)) ∉ (DataType, UnionAll), false),
      raw_clean)
   modules = filter(
      n -> @ignoreerr(typeof(Core.eval(ns, n)) == Module, false),
      all)
   res = map(
      v -> prepend_ns ? string(ns, ".", v) : string(v),
      setdiff(all, modules))
   # deal with modules
   include_modules && append!(res, map(string, modules))
   if recursive
      for m in modules
         new_ns = getfield(ns, m)
         append!(res, lsnames(new_ns, all=false, imported=false, include_modules=include_modules, recursive=true, prepend_ns=true, first_call=false))
      end
   end
   # remove anything which prepended the namespace itself
   if first_call
      return map(
         n -> replace(n, Regex(Printf.@sprintf("^%s\\.", ns)) => ""),
         res)
   else
      return res
   end
end

"""
xref helper: return all identifiers in the given module.
"""
function xref_backend_identifiers(ns)
   raw = names(ns, all=true, imported=true)
   # remove identifiers containing '#' since Elisp doesn't like them
   raw_clean = filter(n -> !occursin(r"#", string(n)), raw)
   vars = filter(
      n -> @ignoreerr(typeof(Core.eval(ns, n)) ∉ (DataType, UnionAll, Module), false),
      raw_clean)
   # convert results to strings
   vars_strs = map(string, vars)
   # strip out leading "Main." from identifiers to avoid confusion
   ns_nomain = replace(string(ns), Regex("^Main\\.") => "")
   vars_strs_nomain = map(
      n -> replace(n, Regex(Printf.@sprintf("^Main\\.%s\\.", ns_nomain)) => ""),
      vars_strs
   )
   return vars_strs_nomain
end

"""
xref helper: return known definitions of given identifier in given namespace.
"""
function xref_backend_definitions(ns, identifier)
   try
      let ms = methods(getproperty(ns, Symbol(identifier))).ms
         # If all definitions point to the same file and line, collapse them
         # into one. This often happens with function default arguments.
         lines = map(m -> m.line, ms)
         files = map(m -> m.file, ms)
         if length(unique(lines)) == 1 && length(unique(files)) == 1
            [(identifier, Base.find_source_file(string(files[1])), lines[1])]
         else
            map(m -> (Printf.@sprintf("%s: %s",
                                      identifier,
                                      join(map(string, m.sig.types[2:end]), ", ")),
                      Base.find_source_file(string(m.file)),
                      m.line),
                ms)
         end
      end
   catch
      nothing
   end
end

"""
FIXME
"""
function completions(ns)
   raw = names(ns, all=true, imported=true)
   # remove identifiers containing '#' since Elisp doesn't like them
   raw_clean = filter(
      n -> !occursin(r"#", string(n)),
      raw
   )
   all = filter(
      n -> @ignoreerr(typeof(Core.eval(ns, n)) ∉ (DataType, UnionAll), false),
      raw_clean)
   modules = filter(
      n -> @ignoreerr(typeof(Core.eval(ns, n)) == Module, false),
      all)
   vars = setdiff(all, modules)
   # loop over modules
   for m in modules
      new_ns = getfield(ns, m)
      new_ns_raw = names(new_ns, all=false, imported=false)
      new_ns_all = map(
         n -> Symbol(string(new_ns, ".", n)),
         new_ns_raw
      )
      append!(vars, new_ns_all)
   end
   # convert results to strings
   vars_strs = map(string, vars)
   # strip out leading "Main." from identifiers to avoid confusion
   ns_nomain = replace(string(ns), Regex("^Main\\.") => "")
   vars_strs_nomain = map(
      n -> replace(n, Regex(Printf.@sprintf("^Main\\.%s\\.", ns_nomain)) => ""),
      vars_strs
   )
   # strip out leading "Base." from identifiers
   # vars_strs_nobase = map(
   #    n -> replace(n, Regex("^Base\\.") => ""),
   #    vars_strs_nomain
   # )
   # strip out leading "Core." from identifiers
   # vars_strs_nocore = map(
   #    n -> replace(n, Regex("^Core\\.") => ""),
   #    vars_strs_nobase
   # )
   # return vars_strs_nocore
   return vars_strs_nomain
end


### --- server code

running = false
server_socket = nothing
client_sockets = []

"""
Start the Snail server.

The server starts a server socket and waits for connections. Connections listen
for commands coming in from clients (i.e., Emacs). Commands are parsed,
dispatched, and evaluated as needed.

Standard output and standard error during evaluation go into the REPL. Errors
during evaluation are captured and sent back to the client as Elisp
s-expressions. Special queries also write back their responses as s-expressions.
"""
function start(port=10011)
   global running = true
   @async begin
      global server_socket = Sockets.listen(port)
      while running
         client = Sockets.accept(server_socket)
         push!(client_sockets, client)
         @async while Sockets.isopen(client)
            command = readline(client, keep=true)
            input = nothing
            try
               input = eval(Meta.parse(command))
               expr = Meta.parse(input.code)
               result = eval_in_module(input.ns, expr)
               # report successful evaluation back to client
               resp = elexpr((Symbol("julia-snail--response-success"),
                              input.reqid,
                              result))
               println(client, resp)
            catch err
               try
                  resp = elexpr((Symbol("julia-snail--response-failure"),
                                 input.reqid,
                                 sprint(showerror, err),
                                 string.(stacktrace(catch_backtrace()))))
                  println(client, resp)
               catch err2
                  if isa(err2, ArgumentError)
                     println("JuliaSnail: ", err2.msg)
                     # client connection was probably closed, clean it up
                     deleteat!(client_sockets, findall(x -> x == client, client_sockets))
                  else
                     println("JuliaSnail: something broke: ", sprint(showerror, err2))
                  end
               end
            end
         end
      end
      close(server_socket)
   end
end

"""
Shut down the Snail server.
"""
function stop()
   global running = false
   close(server_socket)
   for _ in client_sockets
      client = pop!(client_sockets)
      close(client)
   end
end


end
