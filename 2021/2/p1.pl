#!/usr/bin/perl

$horizontal = 0;
$depth = 0;

while(<>) {
    $command = $_;

    if ($command =~ /forward (\d+)/) {
        $horizontal += $1;
    } elsif ($command =~ /up (\d+)/) {
        $depth -= $1;
    } elsif ($command =~ /down (\d+)/) {
        $depth += $1;
    }
}

print $depth * $horizontal . "\n";
