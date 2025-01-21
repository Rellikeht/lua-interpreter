function F(x) return x < 10 end
X = 0
local y = 30

-- while F(X) do
while X < 10 do
  X = X + 1
  y = y - 1
  print(y)
end
print()
print(X, y)
