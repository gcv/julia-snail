import Base64


@testset "parser interface" begin

   s1 = Base64.base64encode(read(joinpath(@__DIR__, "files", "s1.jl"), String))
   s2 = Base64.base64encode(read(joinpath(@__DIR__, "files", "s2.jl"), String))
   s3 = Base64.base64encode(read(joinpath(@__DIR__, "files", "s3.jl"), String))
   s4 = Base64.base64encode(read(joinpath(@__DIR__, "files", "s4.jl"), String))
   s5 = Base64.base64encode(read(joinpath(@__DIR__, "files", "s5.jl"), String))

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

   @testset "code tree" begin
      buf = Base64.base64encode(read(joinpath(@__DIR__, "files", "codetree.jl"), String))
      @test Any[:list,
                (:module, "Alpha", 1, Any[
                   (:module, "Bravo", 15, Any[
                      (:function, "f1(x)", 29),
                      (:module, "Charlie", 54, Any[])]),
                   (:module, "Delta", 80, Any[])]),
                (:module, "Echo", 104, Any[])] ==
         JuliaSnail.CST.codetree(buf)
   end

   @testset "include detection" begin
      # more tests in implicit-modules.el
      @test [:list, "a1.jl", [:list, "Alpha"], "a2.jl", [:list, "Alpha"]] == JuliaSnail.CST.includesin(s4)
      @test [:list, "a1.jl", [:list, "Alpha", "Bravo"]] == JuliaSnail.CST.includesin(s5)
   end

end
