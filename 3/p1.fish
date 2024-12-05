#!/usr/bin/env fish
rg 'mul\((\d+),(\d+)\)' -o -N --replace '$1 * $2' | string join ' + ' | fend
