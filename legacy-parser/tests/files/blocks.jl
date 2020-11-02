module Alpha

module Bravo

macro m1()
   while false
   end
end

struct s1
   x
end

abstract type NewReal <: Number end

primitive type Special <: Integer 8 end

function t1()
   println("hello world")
   for i in 1:10
      if iseven(i)
         println(i)
      end
   end
end

function t2()
   let x = 10, z = 20
      try
         x * z
      catch
         println("oh no")
      finally
         println("ok")
      end
   end
end

function t3()
   x = 10
   begin
      x = 12
      z = quote
         x = 15
      end
   end
end

end

end

module Charlie
end

module Delta

a = 10

begin
   if iseven(a)
      println("even a")
   end
end

function t4(arg1, arg2)
   arg1 * arg2 * a
end

module Echo

let x = 10
   println(x)
end

function t5()
   println("t5")
end

end

end
