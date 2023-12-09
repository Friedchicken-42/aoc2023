include Core

let parse line = 
    List.map int_of_string (String.split_on_char ' ' line)

let rec difference = function
| x :: y :: other -> (y - x) :: difference (y :: other)
| _ -> []

let all_zero = List.for_all (fun x -> x == 0)

let rec calculate lst =
    if all_zero lst then
        0
    else
        let v = List.nth lst ((List.length lst) - 1) in
        let next = difference lst in

        v + calculate next

let () = 
    let lines = read_lines () in
    let values = List.map parse lines in
    let outs = List.map calculate values in
    let out = List.fold_left (+) 0 outs in
    print_int out;
    ()
