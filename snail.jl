module Snail

import Sockets

export start, stop

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
   return fqm.eval(expr) # alternatively: Core.eval(fqm, expr)
end

running = false
server_socket = nothing
client_sockets = []

"""
Start the Snail server.
"""
function start(port=2001)
   global running = true
   @async begin
      global server_socket = Sockets.listen(port)
      while running
         client = Sockets.accept(server_socket)
         push!(client_sockets, client)
         @async while Sockets.isopen(client)
            line = readline(client, keep=true)
            in = eval(Meta.parse(line))
            res = eval_in_module(in.ns, in.expr)
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
