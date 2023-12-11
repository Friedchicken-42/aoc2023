include Core

let all_empty = List.for_all (fun v -> v == '.')

let rec transpose = function
| [] -> []
| [] :: _ -> []
| rows -> List.map List.hd rows :: transpose (List.map List.tl rows)

let extend image =
    let rec horizontally eh = function
    | x :: xs when all_empty x -> x :: eh :: horizontally eh xs
    | x :: xs -> x :: horizontally eh xs
    | _ -> []
    in

    let eh = List.init (List.length image) (fun _ -> '.') in
    let image = horizontally eh image in

    let eh = List.init (List.length image) (fun _ -> '.') in
    let image = horizontally eh (transpose image) in

    transpose image

let get_position image = 
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

let rec calculate_distances positions =
    let distance (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1) in

    let rec inner starting others =
        match others with
        | x :: xs -> distance starting x :: inner starting xs
        | [] -> []
    in

    match positions with
    | x :: xs -> inner x xs @ calculate_distances xs
    | [] -> []

let () = 
    let image = read_lines () in
    let image = List.map chars image in
    let image = extend image in
    let pos = get_position image in
    let distances = calculate_distances pos in
    let sum = List.fold_left (+) 0 distances in
    print_int sum;
    ()
