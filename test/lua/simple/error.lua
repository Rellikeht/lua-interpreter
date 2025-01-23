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

function Loop()
  local i = 0
  while true do
    if FibR(i) > 1000 then error("a") end
    print(FibR(i))
    i = i + 1
  end
end

pcall(Loop)
print("pcalled")
