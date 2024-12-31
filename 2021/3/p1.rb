counts = []

lines = ARGF.readlines()
lines.each do |line|
    line.chomp.chars.each_with_index do |char, i|
        counts[i] ||= 0
        counts[i] += 1 if char == "1"
    end
end

def from_binary binary, digit
    binary <<= 1
    binary += 1 if digit
    return binary
end

gamma = counts.map { |n| n >= lines.length / 2 }.reduce(0, &method(:from_binary))
epsilon = counts.map { |n| n < lines.length / 2 }.reduce(0, &method(:from_binary))

puts gamma * epsilon
