include Core

type part = {
    x: int;
    m: int;
    a: int;
    s: int;
}

type pair = {
    _fn: part -> bool;
    out: string
}

type workflow = {
    name: string;
    pairs: pair list;
    (* last: string; *)
}

let parse_workflow line =
    let (name, rest) = match String.split_on_char '{' line with
    | [n; r] -> (n, r)
    | _ -> failwith "wrong match"
    in

    let pairs = String.split_on_char ',' rest in

    let rec extract acc = function
    | [] -> acc
    | last :: [] -> 
            let out = List.nth (String.split_on_char '}' last) 0 in
            { _fn = (fun _ -> true); out } :: acc
    | pair :: other ->
        Printf.printf "- %s -\n" pair;
        let (name, ch, value, out) = Scanf.sscanf pair "%c%c%d:%s" (fun a b c d -> (a,b,c,d)) in
        let op = match ch with
        | '=' -> (=)
        | '>' -> (>)
        | '<' -> (<)
        | _ -> failwith "Wrong char"
        in
        
        let fn = match name with
        | 'x' -> fun { x; _ } -> op x value
        | 'm' -> fun { m; _ } -> op m value
        | 'a' -> fun { a; _ } -> op a value
        | 's' -> fun { s; _ } -> op s value
        | _ -> failwith "Wrong name"
        in

        extract ({ _fn = fn; out } :: acc) other
    in

    { 
        name;
        pairs = List.rev (extract [] pairs)
    }

let parse_parts line =
    let (x, m, a, s) = Scanf.sscanf 
        line 
        "{x=%d,m=%d,a=%d,s=%d}"
        (fun a b c d -> (a, b, c, d))
    in
    { x; m; a; s }

let parse lines =
    let rec split = function
    | a :: "" :: other -> ([a], other)
    | a :: other ->
        let (o, e) = split other in
        (a :: o, e)
    | [] -> ([], [])
    in

    let (wfs, pts) = split lines in
    let wfs = List.map parse_workflow wfs in
    let map = Hashtbl.create (List.length wfs) in
    List.iter (fun { name; pairs } -> Hashtbl.add map name pairs) wfs;

    let parts = List.map parse_parts pts in

    ( map, parts )

let calculate wfs part =
    let rec find part = function
        | "A" -> Some part
        | "R" -> None
        | key -> 
            let lst = Hashtbl.find wfs key in
            let keys = List.filter (fun { _fn; _ } -> _fn part) lst in
            let nk = List.nth keys 0 in
            find part nk.out
        in

    find part "in"

let () = 
    let lines = read_lines () in
    let (wfs, parts) = parse lines in
    Hashtbl.iter (fun name pairs -> 
        Printf.printf "%s: " name;
        List.iter (fun {out; _} -> Printf.printf "%s " out) pairs;
        print_newline ();
    ) wfs;
    List.iter (fun {x; m; a; s} -> Printf.printf "%d %d %d %d\n" x m a s) parts;
    let accepted = List.filter_map (calculate wfs) parts in
    let out = List.fold_left (fun acc {x; m; a; s} -> x + m + a + s + acc) 0 accepted in
    print_int out;
    ()
