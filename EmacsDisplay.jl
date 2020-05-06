# Some code for displaying Julia plots inside an Emacs buffer
# Design inspired by ElectronDisplay.jl

import Base64.stringmime

struct EmacsDisplayType <: Base.AbstractDisplay
end

function Base.display(d::EmacsDisplayType,::MIME{Symbol("image/svg+xml")}, x)
    imdata =repr("image/svg+xml",x)
    el = elexpr((Symbol("julia-snail--draw-plot"),imdata,Symbol("nil")))
    send_to_client(el)
    return
end

function Base.display(d::EmacsDisplayType,::MIME{Symbol("image/png")}, x)
    imdata =stringmime("image/png",x)
    el = elexpr((Symbol("julia-snail--draw-plot"),imdata,1))
    send_to_client(el)
    return
end




function Base.display(d::EmacsDisplayType,x)
    if showable("image/png", x)
        display(d,"image/png",x)
    elseif showable("image/svg+xml", x)
        display(d,"image/svg+xml",x)
    else
        throw(MethodError(Base.display,(d,x)))
    end
end

"""
    toggle_display()

Turn the Emacs display on/off. If on, any object that can be displayed as png or svg will be displayed inside of Emacs. 
"""
function toggle_display()
    dd = Base.Multimedia.displays[end]
    if  isa(dd,Main.JuliaSnail.EmacsDisplayType)
        popdisplay()
        return "Emacs plotting turned off"
    else
        pushdisplay(JuliaSnail.EmacsDisplayType());
        return "Emacs plotting turned on"
    end
end


# demo code for displaying a stack trace in its own Emacs window
# not plotting per se, disabled
# function Base.display(d::EmacsDisplayType,x :: Array{Base.StackTraces.StackFrame,1})
#     str = repr.(x)
#     el = elexpr((Symbol("julia-snail--show-trace"),str))
#     send_to_client(el)
# end
