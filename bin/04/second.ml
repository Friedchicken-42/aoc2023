include Core

type game = {
    winning: int list;
    numbers: int list;
}

let parse_numbers n = 
    let values = Str.split (Str.regexp " ") n in
    let filtered = List.filter (fun x -> String.length x != 0) values in
    List.map int_of_string filtered

let parse_line line =
    match Str.split (Str.regexp {|\(: \)\|\( | \)|}) line with
    | [_; w; n] -> {
        winning = parse_numbers w;
        numbers = parse_numbers n;
    }
    | _ -> failwith "Wrong match"

let contains l n = List.exists (fun x -> x == n) l

let count_winning game =
    let wins = List.filter (contains game.numbers) game.winning in
    List.length wins

let rec increment diff start steps lst =
    match (lst, start, steps) with
    | (x, _, 0) -> x
    | (x :: xs, 0, n) -> (x + diff) :: increment diff 0 (n - 1) xs
    | (x :: xs, s, n) -> x :: increment diff (s - 1) n xs
    | (_, _, _) -> failwith "Unreachable"

let stuff (index, lst) value = 
    (index + 1, increment ((List.nth lst index) + 1) (index + 1) value lst)

let () = 
    let lines = read_lines () in
    let games = List.map parse_line lines in
    let totals = List.init(List.length games) (fun _ -> 0) in
    let points = List.map count_winning games in
    let (_, out) = List.fold_left stuff (0, totals) points in
    let out = increment 1 0 (List.length out) out in
    print_int (List.fold_left (+) 0 out)
