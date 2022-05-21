## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Formatter

import Base64

Main.JuliaSnail.@with_pkg_env (@__DIR__) begin
   import JuliaFormatter
end

function init()
   # initialization code can go here
end

"""
Format the argument.
"""
function format_data(encodedstr::String)
   try
      str = String(Base64.base64decode(encodedstr))
      JuliaFormatter.format_text(str)
   catch ex # something broke, syntax probably invalid
      println()
      @error ex
      return []
   end
end

end
