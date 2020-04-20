module EmacsDisplay
import Plots.Plot
struct EmacsDisplayType <: Base.AbstractDisplay
end

function displayimg(d::EmacsDisplayType,svg::String)
    println("Bleh")
    el = elexpr((Symbol("show-svg"),svg))
    cmd = `emacsclient --eval $el`
#    @show cmd
    run(cmd)
end

function Base.display(d::EmacsDisplayType,plt::Plot)
    io = IOBuffer();
    println("Blah")
    show(io, MIME("image/svg+xml"), plt)
    sv = String(take!(io))
    @show sv
    displayimg(d,sv)
    close(io)
end

function Base.display(d::EmacsDisplayType, ::MIME{Symbol("image/png")}, x)
    img = stringmime(MIME("image/png"), x)

#    imgdata = "data:image/png;base64, $(img)"

    displayimg(d, x)
end
Base.displayable(d::EmacsDisplayType, ::MIME{Symbol("image/png")}) = true

function Base.display(d::EmacsDisplayType, ::MIME{Symbol("image/svg")}, x)
    img = stringmime(MIME("image/png"), x)

    imgdata = "data:image/png;base64, $(img)"

    displayimg(d, imgdata)
end

Base.displayable(d::EmacsDisplayType, ::MIME{Symbol("image/svg")}) = true
end

