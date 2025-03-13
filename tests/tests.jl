# running the tests:
# $ julia/tests.jl
# or:
# julia> include("tests/tests.jl")

module JuliaSnailTests

using Test

include("../JuliaSnail.jl")

include("./parser.jl")

end
