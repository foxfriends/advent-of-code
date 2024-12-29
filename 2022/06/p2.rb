require 'set'
puts ARGF.read.split("").each_cons(14).find_index { |x| Set.new(x).size == 14 } + 14
