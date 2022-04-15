module ObJulia

function babel_run_and_store(mod::Module, src_file, out_file)
    open(out_file, "w+") do _io
        io = IOContext(_io, :limit => true, :module => mod, :color => true)
        redirect_stdio(stdout=io, stderr=io) do
            result = try
                Core.include(mod, src_file)
            catch err;
                Base.display_error(io, err, Base.catch_backtrace())
            end
            Base.invokelatest() do 
                for (imgtype, ext) ∈ [("image/png", ".png"), ("image/svg+xml", ".svg")]
                    if showable(imgtype, result)
                        tmp = tempname() * ext
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
    println()
    @info "ob-julia evaluated\n"*read(out_file, String)
end


end
