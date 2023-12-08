include Core

type direction = {
    start: string;
    left: string;
    right: string;
}

type steps = {
    start: string;
    reach: int;
    loop: int;
}

let regex = Str.regexp {|\([0-Z]+\) = (\([0-Z]+\), \([0-Z]+\))|}

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
    let starting = List.filter (fun ({ start; _ } : direction) ->
        let chars = List.of_seq (String.to_seq start) in
        List.nth chars 2 == 'A'
    ) nodes in

    let map = Hashtbl.create (List.length nodes) in
    List.iter (fun { start; left; right } -> Hashtbl.add map start (left, right)) nodes;

    (seq, starting, map)

let is_ending c = List.nth (List.of_seq (String.to_seq c)) 2 == 'Z'

let rec count seq skip n current nodes =
    let length = List.length seq in
    let current_dir = List.nth seq (n mod length) in

    match (current, skip) with 
    | (c, false) when is_ending c -> (n, current)
    | (c, _) ->
        let (left, right) = Hashtbl.find nodes c in
        match current_dir with
        | 'L' -> count seq false (n + 1) left nodes
        | 'R' -> count seq false (n + 1) right nodes
        | _ -> failwith "count"

let rec equals = function
    | [] | [_] -> true
    | x :: y :: other -> x = y && equals (y :: other)

let min lst = match lst with
| hd :: other -> 
        List.fold_left 
            (fun (ci, mi, mv) x -> 
                if x < mv then
                    (ci + 1, ci, x)
                else
                    (ci + 1, mi, mv)
            )
        (1, 0, hd) other
| _ -> failwith "min"

let rec update lst index value = match (lst, index, value) with
| (x :: xs, 0, v) -> (x + v) :: xs
| (x :: xs, n, v) -> x :: update xs (n - 1) v
| _ -> failwith "update"

let rec calculate acc pairs =
    if equals acc then
        List.nth acc 0
    else 
        let (_, mi, _) = min acc in
        let acc = update acc mi (List.nth pairs mi).loop in
        calculate acc pairs

let () = 
    let lines = read_lines () in
    let (seq, starting, nodes) = parse lines in


    List.iter (fun ({ start; _ }: direction) -> Printf.printf "%s " start ) starting;
    print_newline ();

    let pairs = List.map (
        fun ({ start; _ } : direction) -> 
            let (r, node) = count seq false 0 start nodes in
            let (l, _) = count seq true 0 node nodes in
            { 
                start; 
                reach = r;
                loop = l;
        }
    ) starting in

    List.iter (fun x -> Printf.printf "%s -> %d + %d\n" x.start x.reach x.loop) pairs;
    let start = List.map (fun { reach; _ } -> reach) pairs in
    let m = calculate start pairs in
    print_int m;
    ()
