include Core

type color = {
    r: int;
    g: int;
    b: int;
}

type game = {
    id: int;
    c: color;
}

let parse_id line = match Str.split (Str.regexp "Game ") line with
    | [x] -> int_of_string x
    | _ -> failwith "Split error"

let parse_single line = 
    let (n, value) = match Str.split (Str.regexp " ") line with
    | [a; b] -> (int_of_string a, b)
    | _ -> failwith "Split error" in

    match value with
    | "red" ->  {r = n; g = 0; b = 0}
    | "green" ->  {r = 0; g = n; b = 0}
    | "blue" ->  {r = 0; g = 0; b = n}
    | _ -> failwith "Wrong color name"

let sum_colors a b = {r = a.r + b.r; g = a.g + b.g; b = a.b + b.b}

let parse_color line = 
    let values = Str.split (Str.regexp ", ") line in
    List.fold_left sum_colors {r = 0; g = 0; b = 0} (List.map parse_single values)

let max a b = {
    r = if a.r > b.r then a.r else b.r;
    g = if a.g > b.g then a.g else b.g;
    b = if a.b > b.b then a.b else b.b;
}

let parse_rgb line = 
    let out = List.fold_left max {r = 0; g = 0; b = 0} 
        (List.map parse_color
            (Str.split (Str.regexp "; ") line)) in

    out

let parse_line line =
    let (str_id, str_game) = match Str.split (Str.regexp ": ") line with
    | [a; b] -> (a, b)
    | _ -> failwith "Split error" in

    {
        id = parse_id str_id; 
        c = parse_rgb str_game;
    }

let possible game =
    game.c.r <= 12 && game.c.g <= 13 && game.c.b <= 14

let () = 
    let lines = read_lines () in
    let res = List.fold_left (+) 0 
        (List.map (fun x ->  x.id)
            (List.filter possible 
                (List.map parse_line lines))) in
    print_int res

