function Ack(M, N)
  if M == 0 then return N + 1 end
  if N == 0 then return Ack(M - 1, 1) end
  return Ack(M - 1, Ack(M, N - 1))
end

print(Ack(4, 0)) -- 13
print(Ack(3, 4)) -- 125
print(Ack(3, 5)) -- 253
print(Ack(3, 6)) -- 509
