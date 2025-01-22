function F(x) return x < 10 end
X = 0
local y = 30

-- while X < 10 do
while F(X) do
  X = X + 1
  y = y - 1
  local z = y + 3
  print(y, z)
end
print()
print(X, y)
