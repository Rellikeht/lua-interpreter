function FibR(n)
  local function helper(i)
    if i <= 0 then
      return 0
    elseif i == 1 then
      return 1
    end
    return helper(i - 1) + helper(i - 2)
  end
  return helper(n)
end

I = 0
while true do
  if FibR(I) > 1000 then error("a") end
  print(FibR(I))
  I = I + 1
end
