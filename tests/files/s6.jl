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

# Plain struct
struct S10
   x
   y
end

# Abstract type
abstract type AbstractPoint end

# Primitive type
primitive type Point24 24 end

# Multiple definitions
function overloaded() 1 end
function overloaded(x) x end
function overloaded(x,y) x+y end
