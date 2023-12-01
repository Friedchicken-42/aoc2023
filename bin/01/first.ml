open List
include Core

let is_digit c =
    let value = Char.code c in
    value >= 48 && value <= 75

let first_number chars =
    match List.find_opt is_digit chars with
    | Some digit -> Char.code digit - 48
    | None -> 0


let find_numbers line = 
    let chars = List.of_seq (String.to_seq line) in
    let first = first_number chars in
    let last = first_number (rev chars) in
    first * 10 + last

let () = 
    let lines = read_lines () in
    let numbers = List.map find_numbers lines in
    let output = List.fold_left (fun a b -> a + b) 0 numbers in
    print_int output
