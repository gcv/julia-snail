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

# Abstract types
abstract type AbstractPoint1 end
abstract type AbstractPoint2 <: Number end

# Primitive types
primitive type Point24 24 end
primitive type Int8 <: Integer 8 end

# Multiple definitions
function overloaded() 1 end
function overloaded(x) x end
function overloaded(x,y) x+y end

# Macro definition
macro sayhello(name)
   return :( println("Hello, ", $name) )
end

# Function with type tags
function load(filepath)::DF.DataFrame
end

function load2(filepath)
end

function load3(filepath::String)::String
end
