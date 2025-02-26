# Nested functions
function outer()
    function inner(x)
        x + 1
    end
    inner(5)
end

# Type parameters
struct GenericPoint{T}
    x::T
    y::T
end

# Multiple definitions
function overloaded() 1 end
function overloaded(x) x end
function overloaded(x,y) x+y end
