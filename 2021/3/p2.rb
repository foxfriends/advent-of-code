lines = ARGF.readlines().map { |l| l.chomp }

def count lines, i
    return lines
        .filter { |line| line.chars[i] == "1" }
        .length
end

oxygen = lines
12.times do |i|
    c = count(oxygen, i) >= oxygen.length / 2.0 ? "1" : "0"
    oxygen = oxygen.filter { |line| line[i] == c }
    break if oxygen.length == 1
end

carbon = lines
12.times do |i|
    c = count(carbon, i) >= carbon.length / 2.0 ? "0" : "1"
    carbon = carbon.filter { |line| line[i] == c }
    break if carbon.length == 1
end


puts(oxygen.first.to_i(2) * carbon.first.to_i(2))
