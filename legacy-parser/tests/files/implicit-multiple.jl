module Alpha

include("a1.jl")

function f1()
   "return value"
end

include("a2.jl")

end


module Bravo

include("b1.jl")
include("b2.jl")

end
