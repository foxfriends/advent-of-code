#!/usr/bin/perl
@lists = (
    ["V", "N", "F", "S", "M", "P", "H", "J"],
    ["Q", "D", "J", "M", "L", "R", "S"],
    ["B", "W", "S", "C", "H", "D", "Q", "N"],
    ["L", "C", "S", "R"],
    ["B", "F", "P", "T", "V", "M"],
    ["C", "N", "Q", "R", "T"],
    ["R", "V", "G"],
    ["R", "L", "D", "P", "S", "Z", "C"],
    ["F", "B", "P", "G", "V", "J", "S", "D"],
);

$i = 0;
while (<>) {
    if ($i++ < 10) { next; }
    last unless /\S/;
    /move (\d+) from (\d+) to (\d+)/;
    @top = splice @{$lists[$2 - 1]}, 0, $1;
    unshift @{$lists[$3 - 1]}, @top;
}

foreach (@lists) {
    print "@$_[0]";
}
print "\n";
