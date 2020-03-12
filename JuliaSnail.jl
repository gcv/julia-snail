## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

import Pkg


module JuliaSnail


import Markdown
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

function elexpr(arg::Markdown.MD)
   elexpr(string(arg))
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
function eval_in_module(fully_qualified_module_name::Array{Symbol}, expr::Expr)
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

"""
Formatter for method signatures, DataType specialized.
"""
function format_method_signature(methodsig::DataType)
   join(map(string, methodsig.types[2:end]), ", ")
end

"""
Formatter for method signatures, generic.
"""
function format_method_signature(methodsig::Any)
   string(methodsig)
end

"""
Return a name split into its module and identifier components.
Example: given a string like "Base.Math.acos", return [Base.Math, "acos"].
"""
function split_name(name::String, ns::Module=Main)
   if match(r"\.", name) == nothing
      return [ns, name]
   end
   components = match(r"(.*?)\.(.*)", name)
   return split_name(string(components[2]), getfield(ns, Symbol(components[1])))
end


### --- introspection helpers

"""
Return names of modules and identifiers in the given namespace.
"""
function lsnames(ns; all=false, imported=false, include_modules=false, recursive=false, prepend_ns=false, first_call=true, pattern=nothing)
   raw = names(ns, all=all, imported=imported)
   # slurp in names of all modules specified with "using", since names() only
   # includes explicitly imported entries
   if first_call && imported && ns ∉ (Core, Base)
      for m in Base.loaded_modules_array()
         if m != ns && m != Main && m != Core && m != Base && isdefined(ns, nameof(m))
            append!(raw, names(m))
         end
      end
   end
   # remove names containing "#" since Elisp doesn't like them
   raw_clean = filter(
      n -> !occursin(r"#", string(n)),
      raw)
   # remove :eval and :include since they are automatically everywhere, along
   # with unhelpful symbols
   raw_clean = filter(
      n -> n ∉ (:eval, :include, :..),
      raw_clean)
   # if ns is Main, remove problematic entries
   if ns == Main
      raw_clean = filter(
         n -> n ∉ (:Base, :Core, :InteractiveUtils, :Pkg, :ans),
         raw_clean)
   end
   # remove self-matches
   raw_clean = filter(
      n -> @ignoreerr(n == :missing || Core.eval(ns, n) ≠ ns, false),
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
   if first_call
      # remove anything which prepended the namespace itself
      full_clean =
         prepend_ns ?
         res :
         map(
            n -> replace(n, Regex(Printf.@sprintf("^%s\\.", ns)) => ""),
            res)
      # apply the pattern
      if pattern ≠ nothing
         pattern_rx = Regex(pattern)
         return filter(
            n -> occursin(pattern_rx, n),
            full_clean)
      else
         return full_clean
      end
   else
      return res
   end
end

"""
Return known definition locations of given identifier in given namespace.
"""
function lsdefinitions(ns, identifier)
   try
      let ms = methods(getproperty(ns, Symbol(identifier))).ms
         # If all definitions point to the same file and line, collapse them
         # into one. This often happens with function default arguments.
         lines = map(m -> m.line, ms)
         files = map(m -> m.file, ms)
         map(m -> (Printf.@sprintf("%s(%s)",
                                   identifier,
                                   format_method_signature(m.sig)),
                   Base.find_source_file(string(m.file)),
                   m.line),
             ms)
      end
   catch
      []
   end
end

# Caches for apropos() function. These are separate from the caching performed
# on the Elisp side by autocompletion to reduce the amount of data transfered
# from Julia to Emacs.
apropos_cached_base = nothing
apropos_cached_core = nothing

"""
Return known definition locations of identifiers matching the pattern in
namespaces loaded in the given namespace.
"""
function apropos(ns, pattern)
   pattern_rx = Regex(pattern)
   names = lsnames(ns, all=true, imported=true, include_modules=false, recursive=true, pattern=pattern)
   # lazy load Base
   global apropos_cached_base
   if apropos_cached_base == nothing
      apropos_cached_base = lsnames(Main.Base, all=true, imported=true, include_modules=false, recursive=true, prepend_ns=true)
   end
   base_filtered = filter(
      n -> occursin(pattern_rx, n),
      apropos_cached_base)
   append!(names, base_filtered)
   # lazy load Core
   global apropos_cached_core
   if apropos_cached_core == nothing
      apropos_cached_core = lsnames(Main.Core, all=true, imported=true, include_modules=false, recursive=true, prepend_ns=true)
   end
   core_filtered = filter(
      n -> occursin(pattern_rx, n),
      apropos_cached_core)
   append!(names, core_filtered)
   # find the definitions of each result
   res::Array{Tuple{String,String,Int32}} = []
   for name in names
      name_ns, name_n = split_name(name, ns)
      append!(res, lsdefinitions(name_ns, name_n))
   end
   return res
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
   global running = false
   global server_socket = Sockets.listen(port)
   let wait_result = timedwait(function(); server_socket.status == Base.StatusActive; end,
                               5.0)
      if :timedout == wait_result
         println(stderr, "ERROR: Timeout while waiting to open server socket for Snail.")
         println(stderr, "ERROR: Snail will not work correctly.")
      elseif :error == wait_result
         println(stderr, "ERROR: Timeout while waiting to open server socket for Snail.")
         println(stderr, "ERROR: Snail will not work correctly.")
      elseif :ok == wait_result
         running = true
      else
         println(stderr, "ERROR: Something broke spectacularly.")
         println(stderr, "ERROR: Snail will not work correctly.")
      end
   end
   @async begin
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
