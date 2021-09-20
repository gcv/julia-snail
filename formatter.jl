#Format regions and buffers via JuliaFormatter
#Inspired by https://codeberg.org/FelipeLema/julia-formatter.el/

function format_data(str::String)
    try
        JuliaFormatter.format_text(str)
    catch #sthg went wrong, syntax probably invalid
        return [] 
    end
end
