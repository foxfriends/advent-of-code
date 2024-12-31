#!/usr/bin/perl

$horizontal = 0;
$depth = 0;
$aim = 0;

while(<>) {
    $command = $_;

    if ($command =~ /forward (\d+)/) {
        $horizontal += $1;
        $depth += $aim * $1;
    } elsif ($command =~ /up (\d+)/) {
        $aim -= $1;
    } elsif ($command =~ /down (\d+)/) {
        $aim += $1;
    }
}

print $depth * $horizontal . "\n";
