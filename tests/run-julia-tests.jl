using Test

include(joinpath(@__DIR__, "..", "JuliaSnail.jl"))

println("JuliaSnail test runner")
println("VERSION=" * string(VERSION))
println("BACKEND=" * (VERSION >= v"1.10" ? "Base.JuliaSyntax" : "Pkg.JuliaSyntax"))

include(joinpath(@__DIR__, "parser.jl"))
