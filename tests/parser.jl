import Base64
using Test


include("../JuliaSnail.jl")


@testset "parser interface" begin

   # TODO: Move these samples into supporting files:

   s1 = Base64.base64encode("""
function f1(); end
""")

   s2 = Base64.base64encode("""
module Alpha
function f1(); end
end
function f2(); end
module Bravo
module Charlie
function f3(); end
end
end
""")

   s3 = Base64.base64encode("""
module Geometry
area_circle(radius) = π * r^2
end
f1() = "f1"
""")

   s4 = Base64.base64encode("""
module Alpha
include("a1.jl")
include("a2.jl")
end
""")

   s5 = Base64.base64encode("""
module Alpha
module Bravo
include("a1.jl")
end
end
""")

   @testset "module detection" begin
      @test [:list] == JuliaSnail.CST.moduleat(s1, 0)
      @test [:list] == JuliaSnail.CST.moduleat(s1, 10)
      @test [:list] == JuliaSnail.CST.moduleat(s2, 0)
      @test [:list, "Alpha"] == JuliaSnail.CST.moduleat(s2, 20)
      @test [:list] == JuliaSnail.CST.moduleat(s2, 50)
      @test [:list, "Bravo", "Charlie"] == JuliaSnail.CST.moduleat(s2, 90)
      @test [:list, "Geometry"] == JuliaSnail.CST.moduleat(s3, 45)
      @test [:list, "Geometry"] == JuliaSnail.CST.moduleat(s3, 50)
      @test [:list] == JuliaSnail.CST.moduleat(s3, 51)
   end

   @testset "block detection" begin
      @test [:list, (), 1, 19, "f1"] == JuliaSnail.CST.blockat(s1, 3)
      @test [:list, tuple("Alpha"), 14, 32, "f1"] == JuliaSnail.CST.blockat(s2, 27)
      @test [:list, (), 37, 55, "f2"] == JuliaSnail.CST.blockat(s2, 50)
      @test [:list, ("Bravo", "Charlie"), 84, 102, "f3"] == JuliaSnail.CST.blockat(s2, 97)
      @test [:list, tuple("Geometry"), 17, 47, "area_circle"] == JuliaSnail.CST.blockat(s3, 25)
   end

   @testset "include detection" begin
      # more tests in implicit-modules.el
      @test [:list, "a1.jl", [:list, "Alpha"], "a2.jl", [:list, "Alpha"]] == JuliaSnail.CST.includesin(s4)
      @test [:list, "a1.jl", [:list, "Alpha", "Bravo"]] == JuliaSnail.CST.includesin(s5)
   end

end
