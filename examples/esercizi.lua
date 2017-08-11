userdata = userdata or {}
u = userdata
u.randomize = false
u.g = 9.8
function u.printvar(var, prec)
   local strvar
   strvar = string.format("%.".. prec .."g",var.value)
   context.unit(strvar .. var.unit)
end
function u.defdervar(vval, unit)
   local ff
   ff = loadstring("return " .. vval)
   return {value = ff(), unit = unit}
end


function u.shuffleTable(t)
   local rand = math.random 
   assert( t, "shuffleTable() expected a table, got nil" )
   local iterations = #t
   local j
   
   for i = iterations, 2, -1 do
      j = rand(i)
      t[i], t[j] = t[j], t[i]
   end
   return t
end

function u.shuffleEsercizi(t)
   assert( t, "shuffleTable() expected a table, got nil" )
   local newtab 
   if u.randomize then newtab= u.shuffleTable(t) else newtab= t end
   for i,k in ipairs(newtab) do 
      tex.print("\\component " .. k)
   end
end

function u.deg2rad(a)
   r = math.pi * a / 180
   return r
end        

function u.cosd(a)
   r = math.cos(u.deg2rad(a))
   return r
end        

function u.sind(a)
   r = math.sin(u.deg2rad(a))
   return r
end        


