module IS = Set.Make(Int)

let rec read_lines () : string list =
    try
        let line = read_line () in
            line :: read_lines ()
        with End_of_file -> []

let slice n str = String.sub str n ((String.length str) - n)

let parse_one str =
    let _ :: n :: [] = String.split_on_char '=' str in int_of_string n

let parse_coord coord_str =
    let x_str :: y_str :: [] = String.split_on_char ',' coord_str in
    let x = parse_one x_str in
    let y = parse_one (slice 1 y_str) in
    (x, y)

let parse line =
    let sensor_str :: beacon_str :: [] = String.split_on_char ':' line in
    let sensor_pos = parse_coord (slice 10 sensor_str) in
    let beacon_pos = parse_coord (slice 21 beacon_str) in
    (sensor_pos, beacon_pos)

let distance x y = abs (x - y)

let manhattan_distance (x1, y1) (x2, y2) = (distance x1 x2) + (distance y1 y2)

let rec range x y = List.init (distance x y) (fun n -> n + x)

let range_at target_y (sensor, beacon) =
    let separation = manhattan_distance sensor beacon in
    let target_distance = distance target_y (snd sensor) in
    let radius = separation - target_distance in
    if radius < 0 then [] else range ((fst sensor) - radius) ((fst sensor) + radius)

let target = 2000000

let sensor_beacons = List.map parse (read_lines ())

let actual_beacons = IS.of_list (List.filter_map (fun (_, (x, y)) -> if x == target then Some y else None) sensor_beacons)

let forbidden = IS.of_list (List.concat_map (range_at target) sensor_beacons)

let () = print_int ((IS.cardinal forbidden) - (IS.cardinal actual_beacons))
