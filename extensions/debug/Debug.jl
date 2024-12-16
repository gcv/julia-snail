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

module Debug

export @enter, @run

Main.JuliaSnail.@with_pkg_env (@__DIR__) begin
    import DebugAdapter
    import Sockets
    import Logging
end

port = 12124
conn = missing
session = missing
server = missing
ready = Channel{Bool}(1)

function init()
    # Logging.global_logger(Logging.SimpleLogger(Logging.Debug))
end

function start()
    global server = Sockets.listen(port)
    Threads.@spawn begin
        while true
            @debug "Listening on port $port"
            global conn = Sockets.accept(server)
            @debug "Accepted connection"
            global session = DebugAdapter.DebugSession(conn)
            # When using the attach request, the terminate request does not work.
            session.capabilities.supportsTerminateRequest = false
            @debug "Starting debug session"
            put!(ready, true)
            DebugAdapter.run(session)
        end
    end
end

function run(mod, code, filepath; stop_on_entry=false)
    if ismissing(server)
        start()
    end
    soe = ":json-false"
    if stop_on_entry
        soe = "t"
    end
    Main.JuliaSnail.send_to_client("""
  (dape `(:request "attach"
          host "localhost"
          port $port
          :type "julia"
          :stopOnEntry $soe))
""")
    # Wait for the session to be ready.
    take!(ready)
    DebugAdapter.debug_code(session, mod, code, filepath)
end

macro enter(command)
    Base.remove_linenums!(command)
    :(run(Main, $(string(command)), $(string(__source__.file)), stop_on_entry=true))
end

macro run(command)
    Base.remove_linenums!(command)
    :(run(Main, $(string(command)), $(string(__source__.file)), stop_on_entry=false))
end
end
