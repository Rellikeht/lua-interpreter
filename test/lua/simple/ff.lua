function f(x) print(x) end
function g(f) return f end
g(f)("Hello")
