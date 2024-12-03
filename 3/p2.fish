rg 'do\(\)|don\'t\(\)|mul\((\d+),(\d+)\)' -o -N | awk '/do\(\)/, /don\'t\(\)/ { print $0 }' | fish p1.fish
