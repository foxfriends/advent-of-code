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

let range_at target_y (sensor, beacon) =
    let separation = manhattan_distance sensor beacon in
    let target_distance = distance target_y (snd sensor) in
    let radius = separation - target_distance in
    if radius < 0 then None else Some(((fst sensor) - radius), ((fst sensor) + radius))

let overlaps (ll, lh) (rl, rh) = ll <= rh + 1 && rl <= lh + 1
let consumes (ll, lh) (rl, rh) = ll >= rl && lh <= rh

let union_range lhs rhs = (min (fst lhs) (fst rhs), max (snd lhs) (snd rhs))

let insert_range ranges range =
    let overlapping = List.filter (overlaps range) ranges in
    let merged = List.fold_left union_range range overlapping in
    merged :: List.filter (fun r -> not (overlaps range r)) ranges

let rec merge_ranges lhs = function
    | [] -> List.sort (fun l r -> (fst l) - (fst r)) lhs
    | range :: rest -> merge_ranges (insert_range lhs range) rest

let any f l = List.fold_left (&&) true (List.map f l)

let max_position = 4000000
let sensor_beacons = List.map parse (read_lines ())
let forbidden_at y = List.fold_left merge_ranges [] [(List.filter_map (range_at y) sensor_beacons)]

exception Not_found
let rec find_solution y = 
    if y > max_position 
    then raise Not_found
    else let forbidden = forbidden_at y in 
        if any (consumes (0, max_position)) forbidden
        then find_solution (y + 1)
        else let (m, lo) :: (hi, n) :: [] = forbidden in (lo + 1, y)

let x, y = find_solution 0
let () = print_int (x * max_position + y)
