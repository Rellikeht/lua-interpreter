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

function FibL(n)
  local a, b = 0, 1
  for _ = 0, n do a, b = b, a + b end
  return a
end

for i = 0, 25 do print(FibR(i), FibL(i)) end
