if 2 == 3 then
  print("bad")
else
  print("good")
end

if "a" == "a" then
  print("good")
else
  print("bad")
end

if nil then
  print("bad")
elseif 1 == 1 then
  print("good")
end

if 1 == 2 then
  print("bad")
elseif 2 == 3 then
  print("bad")
elseif 3 == 5 then
  print("bad")
elseif false and true then
  print("bad")
elseif false or nil then
  print("bad")
else
  print("good")
end
