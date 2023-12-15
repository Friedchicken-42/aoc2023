include Core

type action = {
    name: string;
    hash: int;
    action: char;
    value: int;
}

let hash starting c = (((Char.code c) + starting) * 17) mod 256

let hashing str = List.fold_left hash 0 (chars str)

let is_char c = (Char.code c) >= (Char.code 'a') && (Char.code c) <= (Char.code 'z')

let parse_action str =
    let rec get_name = function
    | x :: xs when is_char x -> 
            let (other, out) = get_name xs in
            (x :: other, out)
    | x :: xs -> ([], x :: xs)
    | [] -> failwith "Wrong input"
    in

    let str = chars str in
    let (name, str) = get_name str in
    let name = String.of_seq (List.to_seq (name)) in

    let action = List.hd str in

    let value = if action = '=' then
        List.tl str |> List.to_seq |> String.of_seq |> int_of_string
    else
        0
    in

    {
        name;
        hash = hashing name;
        action;
        value;
    }

let rec remove action = function
| x :: xs when x.name = action.name -> xs
| x :: xs -> x :: remove action xs
| [] -> []

let rec add action = function
| x :: xs when x.name = action.name -> action :: xs
| x :: xs -> x :: add action xs
| [] -> [action]

let execute boxes action =
    let newlist = if action.action = '-' then
        remove action boxes.(action.hash)
    else if action.action = '=' then
        add action boxes.(action.hash)
    else
        boxes.(action.hash)
    in

    boxes.(action.hash) <- newlist;
    boxes

let () = 
    let line = List.hd (read_lines ()) in
    let values = String.split_on_char ',' line in
    let actions = List.map parse_action values in
    (* List.iter (fun a -> Printf.printf "%d - %s: %c %d\n" a.hash a.name a.action a.value) actions; *)
    let boxes = Array.make 256 [] in
    let boxes = List.fold_left execute boxes actions in
    Array.iteri (fun i b ->
        if List.length b != 0 then (
            Printf.printf "Box %d\n" i;
            List.iter (fun a -> 
                Printf.printf "%d - %s: %c %d\n" a.hash a.name a.action a.value
            ) b;
        )
    ) boxes;

    let out = Array.fold_left (fun (accs, box) lst ->
        let o = List.fold_left (fun (acc, slot) action ->
            (
                (box + 1) * (slot + 1) * action.value + acc,
                slot + 1
            )
        ) (accs, 0) lst in
        (fst o, box + 1)
    ) (0, 0) boxes in
    print_int (fst out);
    ()
