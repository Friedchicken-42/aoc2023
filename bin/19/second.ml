include Core

type condition = {
    name: char option;
    ch: char option;
    value: int64 option;
    out: string;
}

type workflow = {
    name: string;
    conditions: condition list;
}

type range = {
    start: int64;
    last: int64;
}

type part = {
    x: range;
    m: range;
    a: range;
    s: range;
}

let parse_workflow line =
    let (name, rest) = match String.split_on_char '{' line with
    | [n; r] -> (n, r)
    | _ -> failwith "wrong match"
    in

    let conditions = String.split_on_char ',' rest in

    let rec extract acc = function
    | [] -> acc
    | last :: [] -> 
            let out = List.nth (String.split_on_char '}' last) 0 in
            { name = None; ch = None; value = None; out } :: acc
    | pair :: other ->
        let (name, ch, value, out) = Scanf.sscanf pair "%c%c%d:%s" (fun a b c d -> (a,b,c,d)) in
        extract ({ name = Some name; ch = Some ch; value = Some (Int64.of_int value); out } :: acc) other
    in

    { 
        name;
        conditions = List.rev (extract [] conditions)
    }

let parse lines =
    let rec split = function
    | a :: "" :: other -> ([a], other)
    | a :: other ->
        let (o, e) = split other in
        (a :: o, e)
    | [] -> ([], [])
    in

    let (wfs, _) = split lines in
    let wfs = List.map parse_workflow wfs in
    let map = Hashtbl.create (List.length wfs) in
    List.iter (fun { name; conditions } -> Hashtbl.add map name conditions) wfs;

    map

let split_range {start; last} ch (value: int64) =
    match ch with
    | '>' -> ({start = Int64.add value 1L; last}, {start; last = value})
    | '<' -> ({start; last = Int64.sub value 1L}, {start = value; last})
    | _ -> failwith "Wrong char"

let split (part: part) (condition: condition) =
    match condition with
    | { name = Some 'x'; ch = Some ch; value = Some value; _ } -> 
        let (r1, r2) = split_range part.x ch value in
        ({ part with x = r1 }, { part with x = r2 })
    | { name = Some 'm'; ch = Some ch; value = Some value; _ } -> 
        let (r1, r2) = split_range part.m ch value in
        ({ part with m = r1 }, { part with m = r2 })
    | { name = Some 'a'; ch = Some ch; value = Some value; _ } -> 
        let (r1, r2) = split_range part.a ch value in
        ({ part with a = r1 }, { part with a = r2 })
    | { name = Some 's'; ch = Some ch; value = Some value; _ } -> 
        let (r1, r2) = split_range part.s ch value in
        ({ part with s = r1 }, { part with s = r2 })
    | { name = None; _ } -> (part, part)
    | _ -> failwith "wrong name"

let print_part part =
    Printf.printf "{";
    Printf.printf "x=%s~%s, " (Int64.to_string part.x.start) (Int64.to_string part.x.last);
    Printf.printf "m=%s~%s, " (Int64.to_string part.m.start) (Int64.to_string part.m.last);
    Printf.printf "a=%s~%s, " (Int64.to_string part.a.start) (Int64.to_string part.a.last);
    Printf.printf "s=%s~%s"   (Int64.to_string part.s.start) (Int64.to_string part.s.last);
    Printf.printf "}\n";
    ()

let count part =
    Int64.mul
        (Int64.mul
            (Int64.add (Int64.sub part.x.last part.x.start) 1L)
            (Int64.add (Int64.sub part.m.last part.m.start) 1L)
        )
        (Int64.mul
            (Int64.add (Int64.sub part.a.last part.a.start) 1L)
            (Int64.add (Int64.sub part.s.last part.s.start) 1L)
        )

let apply workflows =
    let part = { 
        x = { start = 1L; last = 4000L };
        m = { start = 1L; last = 4000L };
        a = { start = 1L; last = 4000L };
        s = { start = 1L; last = 4000L };
    } in

    let rec inner part = function
    | condition :: cs -> 
        let (p1, p2) = split part condition in
        let v1 = outer p1 condition.out in
        let v2 = inner p2 cs in
        Int64.add v1 v2
    | [] -> 0L

    and outer part = function
    | "A" -> print_part part; count part
    | "R" -> 0L
    | n ->
        let conditions = Hashtbl.find workflows n in
        inner part conditions
    in

    outer part "in" 

let () = 
    let lines = read_lines () in
    let wfs = parse lines in
    Hashtbl.iter (fun name conditions -> 
        Printf.printf "%s: \n" name;
        List.iter (fun pair -> match pair with
            | { name = Some name; ch = Some ch; value = Some value; out } -> 
                    Printf.printf "%c %c %s -> %s\n" name ch (Int64.to_string value) out;
            | { out; _ } -> Printf.printf "Otherwise -> %s\n" out;
        ) conditions;
    ) wfs;
    let out = apply wfs in
    print_endline (Int64.to_string out);
    ()
