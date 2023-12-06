include Core

let is_digit c =
    let value = Char.code c in
    value >= 48 && value <= 57

let rec parse acc value chars = match chars with
| x :: xs when is_digit x -> parse acc (value * 10 + (Char.code x) - 48) xs
| _ :: xs -> parse acc value xs
| [] -> acc @ [value]

let parse_string line = parse [] 0 (List.of_seq (String.to_seq line))

let rec pairs a b = match (a, b) with
| (x :: xs, y :: ys) -> (x, y) :: pairs xs ys
| ([], []) -> []
| _ -> failwith "Wrong size"

let counting (time, dist) =
    let possible = List.init time (fun i -> i * (time - i)) in
    let actual = List.filter (fun x -> x > dist) possible in
    List.length actual

let () = 
    let (times, dist) = match read_lines () with
    | a :: b :: [] -> (a, b)
    | _ -> failwith "Wrong Input" in
    let p = pairs (parse_string times) (parse_string dist) in
    let values = List.map counting p in
    let out = List.fold_left ( * ) 1 values in
    print_int out;
    ()
