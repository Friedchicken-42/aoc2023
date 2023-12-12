include Core

type record = {
    springs: char list;
    conditions: int list;
}

type possible_record = {
    springs: char list list;
    conditions: int list;
}

let parse_record line =
    let (sp, co) = match Str.split (Str.regexp " ") line with
    | a :: b :: _ -> (a, b)
    | _ -> failwith "Wrong match" 
    in
    
    let conditions = Str.split (Str.regexp ",") co in

    ({
        springs = chars sp;
        conditions = List.map int_of_string conditions;
    }: record)

let generate_all (record: record) =
    let rec helper = function
    | x :: xs when x == '?' ->
        List.map (fun l -> '#' :: l) (helper xs) @
        List.map (fun l -> '.' :: l) (helper xs)
    | x :: xs -> List.map (fun l -> x :: l) (helper xs)
    | [] -> [[]]
    in

    ({
        springs = helper record.springs;
        conditions = record.conditions;
    }: possible_record)

let rec check springs conditions started = match (springs, conditions, started) with
| ('.' :: ss, 0 :: cs, _) -> check ss cs false
| ('.' :: ss, [], _) -> check ss [] false
| ('.' :: _, _, true) -> false
| ('.' :: ss, c, false) -> check ss c false
| ('#' :: _, 0 :: [], _) -> false
| ('#' :: _, [], _) -> false
| ('#' :: ss, c :: cs, _) -> check ss ((c - 1) :: cs) true
| ([], 0 :: [], _) -> true
| ([], _ :: [], _) -> false
| ([], _ :: _, _) -> false
| _ -> true

let () = 
    let lines = read_lines () in
    let records = List.map parse_record lines in
    List.iter (fun ({ springs; conditions }: record) ->
        List.iter (Printf.printf "%c ") springs;
        print_newline ();
        List.iter (Printf.printf "%d ") conditions;
        print_newline ();
        ()
    ) records;

    let possiblilities = List.map generate_all records in
    let actual = List.map (fun ({ springs; conditions }: possible_record) ->
        let filtered = List.filter (fun s -> check s conditions false) springs in
        List.length filtered
    ) possiblilities in
    List.iter (Printf.printf "%d\n") actual;
    (* List.iter (fun ({ springs; conditions }: possible_record) ->
        List.iter (Printf.printf "%d ") conditions;
        print_newline ();
        List.iter (fun a -> 
            List.iter (Printf.printf "%c ") a;
            Printf.printf "%b" (check a conditions false);
            print_newline ();
        ) springs;
    ) possiblilities; *)
    let out = List.fold_left (+) 0 actual in
    print_int out;
    ()
