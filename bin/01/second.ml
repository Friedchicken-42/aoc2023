open List
include Core 

let rec starts_with str prefix =
    match (str, prefix) with
    | (_, []) -> true
    | ([], _) -> false
    | (s :: sx, p :: px) when s = p -> starts_with sx px
    | (_, _) -> false

let rec find str tuples =
    match (str, List.find_opt (fun (prefix, _) -> starts_with str prefix) tuples) with
    | (_ :: sx, None) -> find sx tuples
    | (_, Some(_, result)) -> result
    | (_, _) -> 0

let tolist x = List.of_seq(String.to_seq x)

let first_number tuples chars = 
    find chars (List.map (fun (f, s) -> tolist f, s) tuples)

let last_number tuples chars =
    find (List.rev chars) (List.map (fun (f, s) -> (rev (tolist f)), s) tuples)

let find_numbers line = 
    let tuples = [
        ("one", 1); ("two", 2); ("three", 3); ("four", 4); ("five", 5); ("six", 6); ("seven", 7); ("eight", 8); ("nine", 9);
        ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9); ("0", 0)
    ] in 

    let cstr = tolist line in
    let first = first_number tuples cstr in
    let last = last_number tuples cstr in

    first * 10 + last

let () = 
    let lines = read_lines () in
    let numbers = List.map find_numbers lines in
    let output = List.fold_left (fun a b -> a + b) 0 numbers in
    print_int output
