counts = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

File.readlines("input").each do |line|
    line.chomp.chars.each_with_index do |char, i|
        counts[i] += 1 if char == "1"
    end
end

def from_binary binary, digit
    binary <<= 1
    binary += 1 if digit
    return binary
end

gamma = counts.map { |n| n > 500 }.reduce(0, &method(:from_binary))
epsilon = counts.map { |n| n < 500 }.reduce(0, &method(:from_binary))

puts gamma * epsilon
