include Core

let rec transpose = function
| [] -> []
| [] :: _ -> []
| rows -> List.map List.hd rows :: transpose (List.map List.tl rows)

let parse_patterns lines =
    let rec helper (acc: string list list) (base: string list) = function
    | "" :: xs -> helper (acc @ [base]) [] xs
    | x :: xs -> helper acc (base @ [x]) xs
    | [] -> acc @ [base]
    in

    helper [] [] lines

let reflection pattern =
    let rec reflect a b = match (a, b) with
    | (x :: xs, y :: ys) when x = y -> 1 + reflect xs ys
    | _ -> 0
    in

    let rec helper a b = 
    match b with 
    | [] -> []
    | x :: xs -> 
            let size = reflect a b in
            let out = size = min (List.length a) (List.length b) in
            out :: helper (x :: a) xs
    in

    let lst = helper [List.hd pattern] (List.tl pattern) in
    match List.find_index (fun x -> x = true) lst with
    | Some i -> i + 1
    | None -> 0

let summarize pattern = 
    let h = reflection pattern in
    let t = List.map (fun cs -> String.of_seq (List.to_seq cs)) (transpose (List.map chars pattern))in
    let v = reflection t in
    v + 100 * h

let () = 
    let lines = read_lines () in
    let patterns = parse_patterns lines in

    List.iteri (fun i lst ->
        Printf.printf "pattern: %d\n" i;
        List.iter (Printf.printf "%s\n") lst;
        print_newline ();
    ) patterns;

    let summarized = List.map summarize patterns in
    let out = List.fold_left (+) 0 summarized in
    print_int out;
    ()
