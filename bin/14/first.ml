include Core

let tilt_north lines = 
    let rec helper start other = match (start, other) with
    | (s, x :: xs) -> 
        let pairs = List.map2 (fun a b -> match (a, b) with
            | ('.', 'O') -> ('O', '.')
            | _ -> (a, b)
        ) s x in
        let (p, n) = List.split pairs in
        p :: helper n xs
    | (s, _) -> [s]
    in

    let rec outer i lst = match i with
    | 0 -> lst
    | n -> outer (n - 1) (helper (List.hd lst) (List.tl lst))
    in

    outer (List.length lines) lines

let calculate_load lines = 
    List.fold_left
        (fun (accs, index) cs -> (
            List.fold_left (fun acc c ->
                (if c == 'O' then index else 0) + acc
            ) accs cs,
            index + 1
        ))
        (0, 1)
        (List.rev lines)

let () = 
    let lines = List.map chars (read_lines ()) in
    let updated = tilt_north lines in
    List.iter (fun row ->
        List.iter (Printf.printf "%c") row;
        print_newline ();
    ) updated;

    let (load, _) = calculate_load updated in
    print_int load;
    ()
