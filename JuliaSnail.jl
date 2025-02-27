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
import REPL
import Sockets
import REPL.REPLCompletions

export start, stop


### --- package loading support code

"""
XXX: External dependency hack.

This macro allows Snail, or its extensions, to provide a Project.toml file with
Julia dependencies _without_ forcing these extensions to be shipped to the Julia
package registry.

The intent is to activate the package directory containing the Project.toml
file, make sure it loads all the dependencies it needs, but put the directory
containing the relevant Project.toml file at the end of the LOAD_PATH. This
allows Snail's dependencies to be loaded, but not disturb any environments the
user wants to activate.

This is no longer used for the Snail core. Extensions are welcome to use it for
now.

Snail's own dependencies need to be listed first in LOAD_PATH during initial
load, otherwise conflicting versions installed in the Julia global environment
cause conflicts. However, Snail should not be listed first the rest of the time.

Historical example, from back when Snail relied on CSTParser:

```
@with_pkg_env (@__DIR__) begin
   # list all external dependency imports here (from the appropriate Project.toml, either Snail's or an extension's):
   import CSTParser
   # check for dependency API compatibility
   !isdefined(CSTParser, :iscall) &&
     throw(ArgumentError("CSTParser API not compatible, must install Snail-specific version"))
end
```
"""
macro with_pkg_env(dir, action)
   :(
   try
      insert!(LOAD_PATH, 1, $dir)
      $action
   catch err
      if isa(err, ArgumentError)
         if isfile(joinpath($dir, "Project.toml"))
            # force dependency installation
            Main.Pkg.activate($dir)
            Main.Pkg.instantiate()
            Main.Pkg.precompile()
            # activate what was the first entry before Snail was pushed to the head of LOAD_PATH
            Main.Pkg.activate(LOAD_PATH[2])
         end
      end
   finally
      # Remove Snail from the head of the LOAD_PATH and put it at the tail. At this
      # point, all of its own dependencies should be loaded and the user's
      # preferred project should be active.
      deleteat!(LOAD_PATH, 1)
      if isfile(joinpath($dir, "Project.toml"))
         push!(LOAD_PATH, $dir)
      end
   end
   )
end


### --- configuration

module Conf

repl_display_eval_results = false

set!(var, val) = Base.eval(Main.JuliaSnail.Conf, :($var = $val))

end


### --- Elisp s-expression constructor

struct ElispKeyword
   kw::Symbol
end

"""
Construct Elisp expression.

julia> JuliaSnail.elexpr([:mapcar, [:function :exp], (1, 2, 3, "hello")])
"(mapcar (function exp) '(1 2 3 \"hello\"))"

This result can be fed into (eval (read ...)) in Elisp. Note that input tuples
turn into quoted lists, and arrays into ordinary lists. This means an Elisp-side
eval will treat lists as it ordinarily does, where (car ...) is expected to be a
function call.
"""
function elexpr(arg::Tuple)
   Printf.@sprintf("'(%s)", join(map(elexpr, arg), " "))
end

function elexpr(arg::Array)
   isempty(arg) ?
      "nil" :
      Printf.@sprintf("(%s)", join(map(elexpr, arg), " "))
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

function elexpr(arg::Bool)
   arg ? "t" : "nil"
end

function elexpr(arg::Any)
   "nil"
end

function elexpr(arg::ElispKeyword)
   Printf.@sprintf(":%s", arg.kw)
end


### --- Emacs popup display helper

module PopupDisplay

struct Params
   width::Int64
   height::Int64
end

function format(obj, width, height)
   if isnothing(obj)
      return ""
   end
   io = IOBuffer()
   show(IOContext(io,
                  :compact => true,
                  :displaysize => (height, width),
                  :limit => true),
        "text/plain",
        obj)
   String(take!(io))
end

end


### --- evaluation helpers for Julia code coming in from Emacs

struct UndefinedModule <: Exception
   name::Symbol
end

Base.showerror(io::IO, e::UndefinedModule) =
   Printf.@printf(io, "Module %s not defined", e.name)

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
function eval_in_module(fully_qualified_module_name::Array{Symbol}, expr::Union{Symbol, Expr})
   # Work around Julia top-level loading requirements for certain forms; also:
   # https://github.com/gcv/julia-snail/pull/78
   if isa(expr, Expr) && expr.head == :block
      expr.head = :toplevel
   end
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
         try
            Base.root_module(Base.__toplevel__, root)
         catch err2
            if isa(err2, MethodError)
               throw(UndefinedModule(root))
            else
               rethrow(err2)
            end
         end
      else
         rethrow(err)
      end
   end
   for m in fully_qualified_module_name[2:end]
      fqm = getfield(fqm, m)
   end
   # If Revise is being used, force it to update its state; invokelatest is
   # necessary to deal with the World Age problem because Revise was probably
   # loaded after the main Snail loop started running.
   if isdefined(Main, :Revise)
      Base.invokelatest(Main.Revise.revise)
   end
   # go
   return Core.eval(fqm, expr)
end

"""
Change the LineNumberNode instances in the given Expr tree to match real source
file locations.
"""
function expr_change_lnn(expr, filesym, linenum)
   if Expr !== typeof(expr); return; end
   for (i, arg) in enumerate(expr.args)
      if LineNumberNode === typeof(arg)
         expr.args[i] = LineNumberNode(arg.line + linenum - 1, filesym)
      elseif Expr === typeof(arg)
         expr_change_lnn(arg, filesym, linenum)
      end
   end
end

"""
Parse and eval the given tmpfile in the context of the module given by the
modpath array and modify the parsed expression to refer to realfile (instead of
tmpfile) line numbers. Used to evaluate a top-level form in a file while
preserving the original filename and line numbers for xref and stack traces.
"""
function eval_tmpfile(tmpfile, modpath, realfile, linenum,
                      popup_params::Union{Nothing, PopupDisplay.Params}=nothing)
   realfilesym = Symbol(realfile)
   code = read(tmpfile, String)
   exprs = Meta.parse(code)
   # linenum - 1 accounts for the leading "begin" line in tmpfiles
   expr_change_lnn(exprs, realfilesym, linenum - 1)
   result = eval_in_module(modpath, exprs)
   if Conf.repl_display_eval_results && !isnothing(result)
      println()
      @info "Module $modpath\n$result"
   end
   Base.MainInclude.ans = result # update the REPL's magic `ans` variable
   if isnothing(popup_params)
      Main.JuliaSnail.elexpr(true)
   else
      Main.JuliaSnail.elexpr((
         true,
         Main.JuliaSnail.PopupDisplay.format(result, popup_params.width, popup_params.height)
      ))
   end
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
   if isnothing(match(r"\.", name))
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
         [:list;
          map(function(m)
                 floc = functionloc(m) # returns a (file, linenum) tuple
                 (Printf.@sprintf("%s(%s)",
                                  identifier,
                                  format_method_signature(m.sig)),
                  Base.find_source_file(string(floc[1])),
                  floc[2])
              end,
              ms)]
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
   if isnothing(apropos_cached_base)
      apropos_cached_base = lsnames(Main.Base, all=false, imported=true, include_modules=true, recursive=true, prepend_ns=true)
   end
   base_filtered = filter(
      n -> occursin(pattern_rx, n),
      apropos_cached_base)
   append!(names, base_filtered)
   # lazy load Core
   global apropos_cached_core
   if isnothing(apropos_cached_core)
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
      defs = lsdefinitions(name_ns, name_n) # this returns [:list, tuple(...)] for Emacs
      if length(defs) >= 2
         append!(res, defs[2:end])
      end
   end
   return [:list; res]
end

"""
Code completion suggestions for completion string `identifier` in module `mod`.
Completions are provided by the built-in REPL.REPLCompletions.
"""
function replcompletion(identifier, mod)
   cs, _, _ = REPLCompletions.completions(identifier, lastindex(identifier), mod)
   return [:list; REPLCompletions.completion_text.(cs)]
end


### --- JuliaSyntax wrappers

module JStx

import Base64
import Base.JuliaSyntax as JS

"""
Parse a Base64-encoded buffer containing Julia code using JuliaSyntax.

Returns a parsed syntax tree or nothing if parsing fails.

# Arguments
- `encodedbuf`: Base64-encoded string containing Julia source code
"""
function parse(encodedbuf)
   try
      buf = String(Base64.base64decode(encodedbuf))
      JS.parseall(JS.SyntaxNode, buf)
   catch err
      println(err)
      return nothing
   end
end

"""
Return the path from root to the node containing the given byte offset.

Traverses the syntax tree to find nodes containing the offset position,
building a path of nodes from root to leaf.

# Arguments
- `node`: Root JuliaSyntax.SyntaxNode to start traversal from
- `offset`: Byte offset to search for in the syntax tree
- `path`: Accumulator for the path being built (internal use)

Returns an array of named tuples containing:
- expr: The syntax node
- start: Starting byte position
- stop: Ending byte position
"""
function pathat(node::JS.SyntaxNode, offset, path = [(expr=node, start=JS.first_byte(node), stop=JS.last_byte(node))])
   if JS.haschildren(node)
      for child in JS.children(node)
         if JS.first_byte(child) <= offset <= JS.last_byte(child)
            return pathat(child, offset,
                          [path; [(expr=child, start=JS.first_byte(child), stop=JS.last_byte(child))]])
         end
      end
   end
   return path
end

"""
Extract the name from a syntax node based on its kind.

Handles various node types including:
- Modules
- Functions
- Structs (including generic parameters)
- Primitive types
- Abstract types
- Macros

Returns the extracted name as a string, or nothing if the node type is not supported
or does not contain a name.

# Arguments
- `node`: JuliaSyntax.SyntaxNode to extract name from
"""
function nodename(node::JS.SyntaxNode)
   kind = JS.kind(node)
   children = JS.children(node)

   if kind == JS.K"module"
      return string(children[1])

   elseif kind == JS.K"function"
      first = children[1]
      if JS.kind(first) == JS.K"call"
         return string(JS.children(first)[1])
      else
         return string(first)
      end

   elseif kind == JS.K"struct"
      # Handle generic type parameters
      if JS.kind(children[1]) == JS.K"curly"
         curly_children = JS.children(children[1])
         return string(curly_children[1])  # The actual type name
      else
         return string(children[1])  # No type parameters
      end

   elseif kind == JS.K"primitive"
      # Support both these cases:
      # primitive type Point24 24 end
      # primitive type Int8 <: Integer 8 end
      grandchildren = JS.children(children[1])
      if length(grandchildren) > 0
         return string(grandchildren[1])
      else
         return string(children[1])
      end

   elseif kind == JS.K"abstract"
      # Support both these cases:
      # abstract type AbstractPoint1 end
      # abstract type AbstractPoint2 <: Number end
      grandchildren = JS.children(children[1])
      if length(grandchildren) > 0
         return string(grandchildren[1])
      else
         return string(children[1])
      end

   elseif kind == JS.K"macro"
      first = children[1]
      if JS.kind(first) == JS.K"call"
         return string(JS.children(first)[1])
      else
         return string(first)
      end
   end

   return nothing
end

"""
Find the module context at a given byte location in code.

Parses the code and returns a list of module names that enclose the given position,
from outermost to innermost.

# Arguments
- `encodedbuf`: Base64-encoded string containing Julia source code
- `byteloc`: Byte offset to find module context for

Returns an Elisp-compatible list starting with :list followed by module names.
"""
function moduleat(encodedbuf, byteloc)
   tree = JS.parseall(JS.SyntaxNode, encodedbuf; ignore_errors=true)
   path = pathat(tree, byteloc)
   modules = []
   for node in path
      if JS.kind(node.expr) == JS.K"module"
         push!(modules, nodename(node.expr))
      end
   end
   return [:list; modules]
end

"""
Find information about the code block at a given byte location.

Parses the code and returns details about the enclosing block (function, struct, etc.)
at the specified position.

# Arguments
- `encodedbuf`: Base64-encoded string containing Julia source code  
- `byteloc`: Byte offset to find block information for

Returns an Elisp-compatible list containing:
- :list symbol
- Tuple of enclosing module names
- Starting byte position
- Ending byte position
- Block description (e.g. function name)

Returns nothing if no block is found at the location.
"""
function blockat(encodedbuf, byteloc)
   tree = JS.parseall(JS.SyntaxNode, encodedbuf; ignore_errors=true)
   path = pathat(tree, byteloc)
   modules = []
   description = nothing
   start = nothing
   stop = nothing
   for node in path
      if JS.kind(node.expr) == JS.K"module"
         description = nothing
         push!(modules, nodename(node.expr))
      elseif isnothing(description)
         if JS.kind(node.expr) ∈ [JS.K"function", JS.K"macro",
                                  JS.K"struct",
                                  JS.K"abstract", JS.K"primitive"]
            description = nodename(node.expr)
            start = node.start
            stop = node.stop + 1
         elseif JS.kind(node.expr) == JS.K"="
            # Check for function assignment like f() = ...
            children = JS.children(node.expr)
            if length(children) >= 1 && JS.kind(children[1]) == JS.K"call"
               description = string(JS.children(children[1])[1])
               start = node.start
               stop = node.stop + 1
            end
         end
      end
   end
   # result format equivalent to what Elisp side expects
   return isnothing(description) ?
      nothing :
      [:list; tuple(modules...); start; stop; description]
end

"""
Generate a tree representation of code structure.

Parses the code and builds a tree showing the hierarchical structure of:
- Modules
- Functions (with signatures)
- Structs
- Types (abstract and primitive)
- Macros

# Arguments
- `encodedbuf`: Base64-encoded string containing Julia source code

Returns an Elisp-compatible nested list structure starting with :list,
followed by tuples for each code element containing:
- Element type (:module, :function, :struct, :type, :macro)
- Name
- Byte position
- Nested elements (for modules)

Returns nothing if no structure is found.
"""
function codetree(encodedbuf)
   tree = parse(encodedbuf)
   helper = (node, depth = 1) -> begin
      res = []
      for child in JS.children(node)
         kind = JS.kind(child)
         name = nodename(child)
         if name !== nothing
            if kind == JS.K"module"
               push!(res, (:module, name, JS.first_byte(child), helper(child, depth + 1)))
            elseif kind == JS.K"function"
               # Reconstruct function signature from the call node
               if JS.haschildren(child) && JS.kind(JS.children(child)[1]) == JS.K"call"
                  call_node = JS.children(child)[1]
                  sig = String(JS.sourcetext(call_node))
                  push!(res, (:function, sig, JS.first_byte(child)))
               else
                  push!(res, (:function, name * "()", JS.first_byte(child)))
               end
            elseif kind ∈ (JS.K"struct", JS.K"mutable")
               push!(res, (:struct, name, JS.first_byte(child)))
            elseif kind ∈ (JS.K"abstract", JS.K"primitive")
               push!(res, (:type, name, JS.first_byte(child)))
            elseif kind == JS.K"macro"
               push!(res, (:macro, name, JS.first_byte(child)))
            end
         else
            # Flatten results from nested nodes that aren't modules
            if JS.haschildren(child)
               append!(res, helper(child, depth + 1))
            end
         end
      end
      return res
   end
   tree_result = helper(tree)
   return isempty(tree_result) ?
      nothing :
      [:list; tree_result]
end

"""
For a given buffer, return the files `include()`d in each nested module.

Result structure: {
  filename -> [module names]
}

Uses JuliaSyntax to parse the code and find include statements within modules.
Find all include() statements and their enclosing module contexts.

Parses the code and builds a mapping of included files to their module contexts.

# Arguments
- `encodedbuf`: Base64-encoded string containing Julia source code
- `path`: Base path to resolve relative include paths against (default: "")

Returns an Elisp-compatible plist alternating between:
- Full path to included file
- List of enclosing module names at the include point

Returns nothing if no includes are found.
"""
function includesin(encodedbuf, path="")
   tree = parse(encodedbuf)
   results = Dict{String,Vector{String}}()

   helper = (node, modules = Symbol[]) -> begin
      if JS.haschildren(node)
         for child in JS.children(node)
            kind = JS.kind(child)

            # Track module context
            if kind == JS.K"module"
               name = nodename(child)
               if name !== nothing
                  new_modules = [modules; String(name)]
                  helper(child, new_modules)
               end
               continue
            end

            # Check for include calls
            if kind == JS.K"call" && length(JS.children(child)) >= 2
               call_name = JS.children(child)[1]
               if String(JS.sourcetext(call_name)) == "include"
                  # Get filename from the first argument
                  filename_node = JS.children(child)[2]
                  filename = String(JS.sourcetext(filename_node))
                  # Remove quotes
                  filename = replace(filename, r"^\"(.*)\"$" => s"\1")
                  filename = joinpath(path, filename)
                  # Store with current module context
                  results[filename] = String.(copy(modules))
               end
            end

            # Recurse into other nodes
            helper(child, modules)
         end
      end
   end

   helper(tree)

   # Convert to plist for Emacs
   reslist = []
   for (file, modules) in results
      push!(reslist, file)
      push!(reslist, [:list; modules])
   end

   return isempty(reslist) ? nothing : [:list; reslist]
end

end


### --- multimedia support
### Adapted from a PR by https://github.com/dahtah (https://github.com/gcv/julia-snail/pull/21).

module Multimedia

import Base64

struct EmacsDisplay <: Base.AbstractDisplay
end

const EMACS = EmacsDisplay()

function send(img_encoded)
   el = Main.JuliaSnail.elexpr([
      Symbol("julia-snail-multimedia-display"),
      img_encoded
   ])
   Main.JuliaSnail.send_to_client(el)
end

function Base.display(d::EmacsDisplay, ::MIME{Symbol("image/png")}, img_raw)
   send(Base64.stringmime("image/png", img_raw))
end

function Base.display(d::EmacsDisplay, ::MIME{Symbol("image/svg+xml")}, img_raw)
   send(Base64.base64encode(repr("image/svg+xml", img_raw)))
end

function Base.display(d::EmacsDisplay, img)
   supported = ["image/png", "image/svg+xml"]
   for imgtype in supported
      if showable(imgtype, img)
         display(d, imgtype, img)
         return
      end
   end
   # no dice
   throw(MethodError(Base.display, (d, img)))
end

"""
Turn the Emacs multimedia display support on or off. If on, any supported image
type will be displayed in an Emacs buffer.
"""
function display_toggle()
   if EMACS in Base.Multimedia.displays
      popdisplay(EMACS)
      return "Emacs plotting turned off"
   else
      pushdisplay(EMACS)
      return "Emacs plotting turned on"
   end
end

function display_on()
   if EMACS ∉ Base.Multimedia.displays
      pushdisplay(EMACS)
   end
   return true
end

function display_off()
   popdisplay(EMACS)
   return true
end

end


### --- extras: support for extending Snail

module Extensions

"""
Load an extension located in the "extensions" directory. Note that the extension
will load in the context of the JuliaSnail.Extensions module.
"""
function load(path)
   f = Base.Filesystem.joinpath([@__DIR__, "extensions", path...]...)
   include(f)
end

end


### --- task handling code

module Tasks

import Printf

active_tasks_lock = ReentrantLock()
active_tasks = Dict{String, Task}()

function interrupt(reqid)
   if haskey(active_tasks, reqid)
      task = active_tasks[reqid]
      schedule(task, InterruptException(), error=true)
      return [:list, true]
   else
      return [:list, false]
   end
end

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
function start(port=10011; addr="127.0.0.1")
   if VERSION < v"1.10"
      # JuliaSyntax only ships with 1.10.
      # This is an exception-throwing error, and it's placed here so users have
      # a chance to see it before the terminal closes.
      error("ERROR: Julia Snail now requires Julia 1.10 or higher")
   end

   global running = false
   global server_socket = Sockets.listen(Sockets.IPv4(addr), port)
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
   # main loop:
   @async begin
      while running
         client = Sockets.accept(server_socket)
         push!(client_sockets, client)
         @async while Sockets.isopen(client) && !eof(client)
            command = readline(client, keep=true)
            input = nothing
            expr = nothing
            current_reqid = nothing
            try
               input = eval(Meta.parse(command))
               expr = Meta.parse(input.code)
               current_reqid = input.reqid
            catch err
               # probably a parsing error
               resp = elexpr([
                  Symbol("julia-snail--response-failure"),
                  input.reqid,
                  sprint(showerror, err),
                  tuple(string.(stacktrace(catch_backtrace()))...)
               ])
               send_to_client(resp, client)
               continue
            end
            active_task = @task begin # process input
               try
                  result = eval_in_module(input.ns, expr)
                  # report successful evaluation back to client
                  resp = elexpr([
                     Symbol("julia-snail--response-success"),
                     input.reqid,
                     result
                  ])
                  send_to_client(resp, client)
               catch err
                  if isa(err, InterruptException)
                     resp = elexpr([
                        Symbol("julia-snail--response-interrupt"),
                        input.reqid
                     ])
                     send_to_client(resp, client)
                  else
                     try
                        resp = elexpr([
                           Symbol("julia-snail--response-failure"),
                           input.reqid,
                           sprint(showerror, err),
                           tuple(string.(stacktrace(catch_backtrace()))...)
                        ])
                        send_to_client(resp, client)
                     catch err2
                        # internal Snail error or unexpected IO behavior..?
                        println(stderr, "JuliaSnail: something broke in reqid: ", input.reqid, "; ", sprint(showerror, err2))
                     end
                  end
               finally
                  lock(Tasks.active_tasks_lock) do
                     delete!(Tasks.active_tasks, current_reqid)
                  end
               end # process input
            end
            lock(Tasks.active_tasks_lock) do
               Tasks.active_tasks[current_reqid] = active_task
            end
            schedule(active_task)
         end # async while loop for client connection
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
   if isnothing(client_socket)
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
   if !isopen(client_socket)
      throw("Something broke: client socket is already closed")
   end
   println(client_socket, expr)
end

end
