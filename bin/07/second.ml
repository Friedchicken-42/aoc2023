include Core

type hand_bid = {
    hand: char list; 
    bid: int;
}

let parse_hand s = List.of_seq (String.to_seq s)

let parse line =
    let (numbers, bid) = match Str.split (Str.regexp " ") line with
    | a :: b :: [] -> (a, b)
    | _ -> failwith "Wrong match"
    in
    { hand = parse_hand numbers; bid = int_of_string bid }

let convert_single = function
| 'J' -> -1
| '2' -> 0
| '3' -> 1
| '4' -> 2
| '5' -> 3
| '6' -> 4
| '7' -> 5
| '8' -> 6
| '9' -> 7
| 'T' -> 8
| 'Q' -> 9
| 'K' -> 10
| 'A' -> 11
| _ -> failwith "Wrong char"

let convert hand = 
    let rec helper hand n = match (hand, n) with
    | (_, 0) -> []
    | (x :: xs, n) -> convert_single x :: helper xs (n - 1)
    | _ -> failwith "helper"
    in
    helper hand 5

let rec increment acc x = match (acc, x) with
    | (x :: xs, 0) -> (x + 1) :: xs
    | (x :: xs, -1) -> (x + 1) :: increment xs (-1)
    | ([], -1) -> []
    | (x :: xs, n) -> x :: increment xs (n - 1)
    | _ -> failwith "helper"

let count hand = 
    let acc = List.init 13 (fun _ -> 0) in
    let values = List.fold_left increment acc hand in
    List.rev (List.sort compare values)

let strength hand = 
    let values = count hand in
    let joker = List.exists (fun x -> x == -1) hand in

    match values with
    | 5 :: _ -> 6
    | 4 :: _ -> 5
    | 3 :: 3 :: 1 :: _ -> 4 (* full house with joker *)
    | 3 :: 2 :: _ -> if joker then 3 else 4
    | 3 :: _ -> 3
    | 2 :: 2 :: _ -> if joker then 1 else 2
    | 2 :: _ -> 1
    | 1 :: _ -> 0
    | _ -> failwith "strength"
    
let rec compare_first n h1 h2 = match (n, h1, h2) with
| (x, a :: o1, b :: o2) -> 
    if a == b then
        compare_first (x - 1) o1 o2
    else if a > b then
        1
    else
        -1
| (0, _, _) -> 0
| _ -> failwith "compare_first"

let compare_hand h1 h2 = 
    let h1 = convert h1 in
    let h2 = convert h2 in

    let strength1 = strength h1 in
    let strength2 = strength h2 in

    if strength1 == strength2 then
        let out = compare_first 5 h1 h2 in
        out
    else if strength1 > strength2 then
         1
    else 
        -1

let () = 
    let lines = read_lines () in
    let hands = List.map parse lines in
    let sorted = List.sort (fun { hand = h1; _ } { hand = h2; _ } -> compare_hand h1 h2) hands in

    List.iter (fun { hand; bid; } -> 
        Printf.printf "bid: %6d;   strenght: %2d;   hand: " bid (strength (convert hand)); 
        List.iter print_char hand;
        print_string "    ";
        List.iter (Printf.printf "%d") (count (convert hand));
        print_newline ();
    ) sorted;

    let (out, _) = List.fold_left (fun (acc, index) { bid; _ } -> (acc + (bid * index), index + 1)) (0, 1) sorted in
    print_int out;
    ()
