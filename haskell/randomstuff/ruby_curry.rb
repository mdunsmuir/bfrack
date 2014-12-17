times = lambda { |x, y| x * y }.curry

puts [3, 5, 7].map(&times[2]).inspect
