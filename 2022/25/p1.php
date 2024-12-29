<?php

$translation = array(
    '2' => 2,
    '1' => 1,
    '0' => 0,
    '-' => -1,
    '=' => -2,
);

$sum = 0;
while ($line = fgets(STDIN)) {
    $snafu = trim($line);

    // array second param
    $digits = array_map(fn($char) => $translation[$char], str_split($snafu));
    // array first param
    $decimal = array_reduce($digits, fn($carry, $item) => $carry * 5 + $item, 0);
    // now isn't that just lovely
    $sum += $decimal;
}

$digits = array();
while ($sum) {
    $digit = $sum % 5;
    array_unshift($digits, $digit);
    $sum = ($sum - $digit) / 5;
}

$carry = false;
for ($i = count($digits) - 1; $i >= 0; $i--) {
    if ($carry) {
        $digits[$i] += 1;
    }
    if ($digits[$i] >= 3) {
        $digits[$i] -= 5;
        $carry = true;
    } else {
        $carry = false;
    }
}
if ($carry) {
    array_unshift($digits, 1);
}

$digits = array_map(fn($digit) => array_flip($translation)[$digit], $digits);
$snafu = implode("", $digits);
echo "$snafu\n";

?>
