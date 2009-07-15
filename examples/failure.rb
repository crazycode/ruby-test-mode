def recurse(i)
  if i > 0
    reflect(i-1)
  else
    raise(StandardError.new("Intentionally unwind"))
  end
end

def reflect(n)
  recurse(n - 1)
end
