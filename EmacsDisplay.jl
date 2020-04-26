import Base64.stringmime

struct EmacsDisplayType <: Base.AbstractDisplay
end

function EmacsDisplay()
    EmacsDisplayType()
end

function Base.display(d::EmacsDisplayType,::MIME{Symbol("image/svg+xml")}, x)
    imdata =repr("image/svg+xml",x)
    el = elexpr((Symbol("julia-snail--draw-plot"),imdata,Symbol("nil")))
    pipeline(`emacsclient --eval $el`,stdout=devnull) |> run;
    return
end

function Base.display(d::EmacsDisplayType,::MIME{Symbol("image/png")}, x)
    imdata =stringmime("image/png",x)
    el = elexpr((Symbol("julia-snail--draw-plot"),imdata,1))
    pipeline(`emacsclient --eval $el`,stdout=devnull) |> run;
    return
end

function Base.display(d::EmacsDisplayType,x :: Array{Base.StackTraces.StackFrame,1})
    str = repr.(x)
    el = elexpr((Symbol("julia-snail--show-trace"),str))
    pipeline(`emacsclient --eval $el`,stdout=devnull) |> run;
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
