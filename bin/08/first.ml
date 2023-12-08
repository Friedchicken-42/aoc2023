include Core

type direction = {
    start: string;
    left: string;
    right: string;
}

let regex = Str.regexp {|\([A-Z]+\) = (\([A-Z]+\), \([A-Z]+\))|}

let parse_node line = 
    if Str.string_match regex line 0 then
        {
            start = Str.matched_group 1 line;
            left = Str.matched_group 2 line;
            right = Str.matched_group 3 line;
        }
    else
        failwith "Wrong regex match"

let parse lines =
    let (seq, nodes) = match lines with
    | a :: _ :: b -> (a, b)
    | _ -> failwith "Wrong match"
    in

    let seq = List.of_seq (String.to_seq seq) in

    let nodes = List.map parse_node nodes in
    List.iter (fun x -> Printf.printf "%s -> %s - %s\n" x.start x.left x.right) nodes;

    let map = Hashtbl.create (List.length nodes) in
    List.iter (fun { start; left; right } -> Hashtbl.add map start (left, right)) nodes;

    (seq, map)

let rec count seq n current nodes =
    Printf.printf "curr -> %s\n" current;
    let length = List.length seq in
    let current_dir = List.nth seq (n mod length) in

    match current with 
    | "ZZZ" -> n
    | c ->
        let (left, right) = Hashtbl.find nodes c in
        match current_dir with
        | 'L' -> count seq (n + 1) left nodes
        | 'R' -> count seq (n + 1) right nodes
        | _ -> failwith "count"

let () = 
    let lines = read_lines () in
    let (seq, nodes) = parse lines in

    List.iter print_char seq;
    print_newline ();


    let out = count seq 0 "AAA" nodes in
    print_int out;
    ()
