import Base64


@testset "parser interface" begin

   s1 = Base64.base64encode(read(joinpath(@__DIR__, "files", "s1.jl"), String))
   s2 = Base64.base64encode(read(joinpath(@__DIR__, "files", "s2.jl"), String))
   s3 = Base64.base64encode(read(joinpath(@__DIR__, "files", "s3.jl"), String))
   s4 = Base64.base64encode(read(joinpath(@__DIR__, "files", "s4.jl"), String))
   s5 = Base64.base64encode(read(joinpath(@__DIR__, "files", "s5.jl"), String))
   s6 = Base64.base64encode(read(joinpath(@__DIR__, "files", "s6.jl"), String))

   @testset "module detection" begin
      @test [:list] == JuliaSnail.JStx.moduleat(s1, 0)
      @test [:list] == JuliaSnail.JStx.moduleat(s1, 10)
      @test [:list] == JuliaSnail.JStx.moduleat(s2, 0)
      @test [:list, "Alpha"] == JuliaSnail.JStx.moduleat(s2, 20)
      @test [:list] == JuliaSnail.JStx.moduleat(s2, 50)
      @test [:list, "Bravo", "Charlie"] == JuliaSnail.JStx.moduleat(s2, 90)
      @test [:list, "Geometry"] == JuliaSnail.JStx.moduleat(s3, 45)
      @test [:list, "Geometry"] == JuliaSnail.JStx.moduleat(s3, 50)
      @test [:list] == JuliaSnail.JStx.moduleat(s3, 51)
   end

   @testset "block detection" begin
      @test [:list, (), 1, 19, "f1"] == JuliaSnail.JStx.blockat(s1, 3)
      @test [:list, tuple("Alpha"), 14, 32, "f1"] == JuliaSnail.JStx.blockat(s2, 27)
      @test [:list, (), 37, 55, "f2"] == JuliaSnail.JStx.blockat(s2, 50)
      @test [:list, ("Bravo", "Charlie"), 84, 102, "f3"] == JuliaSnail.JStx.blockat(s2, 97)
      @test [:list, tuple("Geometry"), 17, 47, "area_circle"] == JuliaSnail.JStx.blockat(s3, 25)
   end

   @testset "code tree" begin
      buf = Base64.base64encode(read(joinpath(@__DIR__, "files", "codetree.jl"), String))
      @test Any[:list,
                (:module, "Alpha", 1, Any[
                   (:module, "Bravo", 15, Any[
                      (:function, "f1(x)", 29),
                      (:module, "Charlie", 54, Any[])]),
                   (:module, "Delta", 80, Any[])]),
                (:module, "Echo", 104, Any[])] ==
         JuliaSnail.JStx.codetree(buf)
   end

   @testset "include detection" begin
      # more tests in implicit-modules.el
      @test [:list, "a1.jl", [:list, "Alpha"], "a2.jl", [:list, "Alpha"]] == JuliaSnail.JStx.includesin(s4)
      @test [:list, "a1.jl", [:list, "Alpha", "Bravo"]] == JuliaSnail.JStx.includesin(s5)
   end

   @testset "Special syntax cases" begin
      # Test nested function (only outer)
      @test [:list, (), 20, 97, "outer"] == JuliaSnail.JStx.blockat(s6, 70)
      @test [:list, (), 20, 97, "outer"] == JuliaSnail.JStx.blockat(s6, 85)

      # Test struct with parameters
      @test [:list, (), 117, 161, "GenericPoint"] == JuliaSnail.JStx.blockat(s6, 150)

      # Test plain struct
      @test [:list, (), 178, 202, "S10"] == JuliaSnail.JStx.blockat(s6, 190)

      # Test abstract type
      @test [:list, (), 221, 253, "AbstractPoint1"] == JuliaSnail.JStx.blockat(s6, 235)
      @test [:list, (), 254, 296, "AbstractPoint2"] == JuliaSnail.JStx.blockat(s6, 268)

      # Test primitive types
      @test [:list, (), 316, 345, "Point24"] == JuliaSnail.JStx.blockat(s6, 331)
      @test [:list, (), 346, 382, "Int8"] == JuliaSnail.JStx.blockat(s6, 361)

      # Test multiple definitions
      @test [:list, (), 407, 434, "overloaded"] == JuliaSnail.JStx.blockat(s6, 416)
      @test [:list, (), 435, 463, "overloaded"] == JuliaSnail.JStx.blockat(s6, 444)
      @test [:list, (), 464, 496, "overloaded"] == JuliaSnail.JStx.blockat(s6, 473)

      # Test macro
      @test [:list, (), 517, 582, "sayhello"] == JuliaSnail.JStx.blockat(s6, 523)

      # Test function with return type
      @test [:list, (), 610, 651, "load"] == JuliaSnail.JStx.blockat(s6, 619)
      @test [:list, (), 653, 681, "load2"] == JuliaSnail.JStx.blockat(s6, 662)
      @test [:list, (), 683, 727, "load3"] == JuliaSnail.JStx.blockat(s6, 692)
   end

end
