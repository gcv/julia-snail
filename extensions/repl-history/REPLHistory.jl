module REPLHistory

import REPL

function init()
   println("# activating Snail extension: REPLHistory\n")
end

"""
Return the last `n` items from the REPL history.
Adapted from https://github.com/carstenbauer/SaveREPL.jl.
"""
function replhistory(n::Int)
   h = reverse(readlines(REPL.find_hist_file()))
   entries = String[]
   i = 1
   N = length(h)
   c = 0
   while i <= N && c < n
      line = h[i]
      cmdlines = String[]
      while startswith(line, "\t")
         push!(cmdlines, replace(line, "\t" => ""; count=1))
         i += 1
         line = h[i]
      end
      command = join(reverse(cmdlines), "\n")
      contains(line, "# mode:") || warn("wrong order: expected mode")
      mode = replace(chomp(line), "# mode: " => "")
      i += 1
      line = h[i]
      contains(line, "# time: ") || warn("wrong order: expected time")
      time = replace(chomp(line), "# time: " => "")
      i += 1
      contains(mode, "julia") || continue
      #push!(entries, REPLEntry(time, mode, command))
      push!(entries, command)
      c += 1
   end
   return [:list; reverse(entries)]
end

end
