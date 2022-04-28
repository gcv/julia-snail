module Popup
function init()
    # initialization code can go here
end

#convert obj to text, to display in emacs popup
function display_obj(obj)
    io = IOBuffer();
    show(IOContext(io,:compact=>true,:displaysize=>(10,10),:limit=>true),"text/plain",obj);
    String(take!(io));
end

end
