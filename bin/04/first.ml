include Core

type game = {
    (* _id: int; *)
    winning: int list;
    numbers: int list;
}

let parse_numbers n = 
    let values = Str.split (Str.regexp " ") n in
    let filtered = List.filter (fun x -> String.length x != 0) values in
    List.map int_of_string filtered

let parse_line line =
    (* match Str.split (Str.regexp {|\(Card \)\|\(: \)\|\( | \)|}) line with *)
    match Str.split (Str.regexp {|\(: \)\|\( | \)|}) line with
    | [_i; w; n] -> {
        winning = parse_numbers w;
        numbers = parse_numbers n;
    }
    | _ -> failwith "Wrong match"

let contains l n = List.exists (fun x -> x == n) l

let count_winning game =
    let wins = List.filter (contains game.numbers) game.winning in
    List.length wins

let rec count_points n = match n with
    | 0 -> 0
    | 1 -> 1
    | x -> count_points (x - 1) * 2

let () = 
    let lines = read_lines () in
    let games = List.map parse_line lines in
    let calculate_points g = count_points (count_winning g) in
    let points = List.map calculate_points games in
    let total = List.fold_left (+) 0 points in
    print_int total
