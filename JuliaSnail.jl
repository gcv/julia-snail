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

# a quick hack to allow using external dependencies
# without making JuliaSnail a package (but we should probably make it a package)
Pkg.activate(@__DIR__)

module JuliaSnail


import Markdown
import Printf
import REPL
import Sockets
import REPL.REPLCompletions
import CSTParser

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
   # NB: The raw_clean to all_names filter used to say "∉ (DataType, UnionAll)".
   # This had to do with removing various names in namespaces which contained
   # "#" and "##" symbols. They are now filtered higher up by string. The
   # DataType filter killed autocompletion for structs. Leaving the UnionAll
   # filter alone for now, since it does not seem to do any harm.
   all_names = filter(
      n -> @ignoreerr(typeof(Core.eval(ns, n)) ≠ UnionAll, false),
      raw_clean)
   module_names = filter(
      n -> @ignoreerr(typeof(Core.eval(ns, n)) == Module, false),
      all_names)
   res = map(
      v -> prepend_ns ? string(ns, ".", v) : string(v),
      setdiff(all_names, module_names))
   # deal with modules
   include_modules && append!(res, map(string, module_names))
   if recursive
      for m in module_names
         new_ns = getfield(ns, m)
         append!(res, lsnames(new_ns, all=all, imported=false, include_modules=include_modules, recursive=true, prepend_ns=true, first_call=false))
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
      apropos_cached_base = lsnames(Main.Base, all=false, imported=true, include_modules=true, recursive=true, prepend_ns=true)
   end
   base_filtered = filter(
      n -> occursin(pattern_rx, n),
      apropos_cached_base)
   append!(names, base_filtered)
   # lazy load Core
   global apropos_cached_core
   if apropos_cached_core == nothing
      apropos_cached_core = lsnames(Main.Core, all=false, imported=true, include_modules=true, recursive=true, prepend_ns=true)
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

"""
    replcompletion(identifier,mod)
Code completion suggestions for completion string `identifier` in module `mod`.
Completions are provided by the built-in REPL.REPLCompletions.
"""
function replcompletion(identifier,mod)
    cs,_,_ = REPLCompletions.completions(identifier, length(identifier), mod)
    return REPLCompletions.completion_text.(cs)
end

### playing around with CSTParser

# I took this function from LangaugeServer.jl
function get_expr(x, offset, pos = 0, ignorewhitespace = false)
    if pos > offset
        return nothing
    end
    if x.args !== nothing && CSTParser.typof(x) !== CSTParser.NONSTDIDENTIFIER
        for a in x.args
            if pos < offset <= (pos + a.fullspan)
                return get_expr(a, offset, pos, ignorewhitespace)
            end
            pos += a.fullspan
        end
    elseif pos == 0
        return x
    elseif (pos < offset <= (pos + x.fullspan))
        ignorewhitespace && pos + x.span < offset && return nothing
        return x
    end
end

function get_module(x,offset)
    ms = []
    a = CSTParser.parentof(get_expr(x,offset))
    while !isnothing(a)
        CSTParser.defines_module(a) && push!(ms,CSTParser.get_name(a).val)
        a = CSTParser.parentof(a)
    end
    return reverse(ms)
end

function module_atpoint(file, point)
    return get_module(CSTParser.parse(read(file,String)), point)
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
               send_to_client(resp, client)
            catch err
               try
                  resp = elexpr((Symbol("julia-snail--response-failure"),
                                 input.reqid,
                                 sprint(showerror, err),
                                 string.(stacktrace(catch_backtrace()))))
                  send_to_client(resp, client)
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

"""
Send data back to a client.

For Emacs, this should be a string containing Elisp which Emacs will eval. It
can be constructed from Julia data structures using elexpr.

The client_socket parameter is optional. If specified, it will send data to that
client. If omitted, then send_to_client will look at the client socket list. If
that list only has one entry, it will send the data to that socket. If that list
has multiple entries, send_to_client will prompt the user at the REPL to select
which client should receive the message.
"""
function send_to_client(expr, client_socket=nothing)
   if client_socket == nothing
      if isempty(client_sockets)
         throw("No client connections available")
      elseif 1 == length(client_sockets)
         client_socket = first(client_sockets)
      else
         # force the user to choose the client socket
         options = map(
            function(cs)
            gsn = Sockets.getpeername(cs)
            Printf.@sprintf("%s:%d", gsn[1], gsn[2])
            end,
            client_sockets
         )
         menu = REPL.TerminalMenus.RadioMenu(options)
         choice = REPL.TerminalMenus.request("Send expression to which Snail client?", menu)
         client_socket = client_sockets[choice]
         # TODO: Ask if this should be the default socket from now on, and save
         # in default_client_socket variable. Use default_client_variable
         # automatically if it is set. Clean up default_client_variable on
         # disconnect.
      end
   end
   println(client_socket, expr)
end


end

Pkg.activate(".")
