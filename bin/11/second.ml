include Core

let all_empty = List.for_all (fun v -> v == '.')

let rec transpose = function
| [] -> []
| [] :: _ -> []
| rows -> List.map List.hd rows :: transpose (List.map List.tl rows)

let get_positions image = 
    let rec inner a b = function
    | x :: xs when x == '#' -> (a, b) :: inner a (b + 1) xs
    | _ :: xs -> inner a (b + 1) xs
    | [] -> []
    in

    let rec outer a = function
    | x :: xs -> inner a 0 x @ outer (a + 1) xs
    | [] -> []
    in

    outer 0 image 

let get_offsets image =
    let rec inner i get_pos = function
    | x :: xs when all_empty x -> get_pos i :: inner (i + 1) get_pos xs
    | _ :: xs -> inner (i + 1) get_pos xs
    | [] -> []
    in

    inner 0 (fun v -> (v, -1)) image @ inner 0 (fun v -> (-1, v)) (transpose image)

let rec get_points (y1, x1) (y2, x2) =
    if y1 < y2 then
        (y1, x1) :: get_points (y1 + 1, x1) (y2, x2)
    else if y1 > y2 then
        (y1, x1) :: get_points (y1 - 1, x1) (y2, x2)
    else if x1 < x2 then
        (y1, x1) :: get_points (y1, x1 + 1) (y2, x2)
    else if x1 > x2 then
        (y1, x1) :: get_points (y1, x1 - 1) (y2, x2)
    else
        [(y1, x1)]

let extend = 1000000

let rec calculate_distances positions offsets =
    let calculate_distance a b =
        let points = get_points a b in 
        let offs = List.filter (fun (y1, x1) -> 
            List.exists (fun (y2, x2) -> y2 == -1 && x2 == x1 || x2 == -1 && y2 == y1) offsets
        ) points in
        (List.length points) + (List.length offs) * (extend - 1) - 1
    in

    let rec inner starting others =
        match others with
        | x :: xs -> calculate_distance starting x :: inner starting xs
        | [] -> []
    in

    match positions with
    | x :: xs -> inner x xs @ calculate_distances xs offsets
    | [] -> []

let () = 
    let image = read_lines () in
    let image = List.map chars image in
    let positions = get_positions image in
    let offsets = get_offsets image in
    let distances = calculate_distances positions offsets in
    let sum = List.fold_left (+) 0 distances in
    print_int sum;
    ()
