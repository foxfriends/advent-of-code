rg 'mul\((\d+),(\d+)\)' -o -N --replace '$1 * $2' | xargs -I _ fend '_' | string join ' + ' | fend
