require 'set'
puts ARGF.read.split("").each_cons(4).find_index { |x| Set.new(x).size == 4 } + 4
