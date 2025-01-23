function F()
  local i = 1
  while true do
    if i > 1000 then return i end
    i = 2 * i
  end
end

print(F())
