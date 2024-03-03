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

module ObJulia

function maybe_redirect_stderr_stdout(f, io, flag)
    if flag
        redirect_stderr(() -> redirect_stdout(f, io), io)
    else
        f()
    end
end

function babel_run_and_store(mod::Module, src_file, out_file, resource_directory,
                             use_error_pane::Bool,
                             mirror_to_repl::Bool,
                             capture_io::Bool)
    open(out_file, "w+") do _io
        io = IOContext(_io, :limit => true, :module => mod, :color => true)
        result = maybe_redirect_stderr_stdout(io, capture_io) do
            result = try
                Core.include(mod, src_file)
            catch err;
                if use_error_pane
                    flush(_io)
                    rethrow()
                else
                    Base.display_error(io, err, Base.catch_backtrace())
                end
            end
        end
        maybe_redirect_stderr_stdout(io, true) do
            Base.invokelatest() do
                for (imgtype, ext) ∈ [("image/png", ".png"), ("image/svg+xml", ".svg")]
                    if showable(imgtype, result)
                        tmp = tempname(mkpath(resource_directory); cleanup=false) * ext
                        open(tmp, "w+") do io
                            show(io, imgtype, result) # Save the image to disk
                        end
                        println(io, "[[file:$tmp]]") # print out an org-link to the saved image
                        result = nothing
                    end
                end
                isnothing(result) || show(io, "text/plain", result)
            end
        end
    end
    if mirror_to_repl
        println()
        @info "ob-julia evaluated in module $mod\n"*read(out_file, String)
    end
end

end
