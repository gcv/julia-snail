module Snail

import Sockets

export start, stop

"""
Call eval on expr in the context of the module given by the
fully_qualified_module_name, which has the form of an array of symbols. The
first element of the array is always implicitly :Main, and can be omitted.

```julia
eval_module_expr([:One, :Two, :Three], :(x = 3 + 5))
```
is equivalent to
```julia
Main.One.Two.Three.eval(:(x = 3 + 5))
```
"""
function eval_module_expr(fully_qualified_module_name::Array{Symbol, 1}, expr::Expr)
   fqm = Main.eval(first(fully_qualified_module_name))
   for m in fully_qualified_module_name[2:end]
      fqm = fqm.eval(m)
   end
   return fqm.eval(expr)
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
            res = eval_module_expr(in.ns, in.expr)
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
