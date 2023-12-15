include Core

let hash starting c = (((Char.code c) + starting) * 17) mod 256

let hashing str = List.fold_left hash 0 (chars str)

let () = 
    let line = List.hd (read_lines ()) in
    let values = String.split_on_char ',' line in
    (* List.iter (fun s -> Printf.printf "%s - %d\n" s (hashing s)) values; *)
    let hashed = List.map hashing values in
    let out = List.fold_left (+) 0 hashed in
    print_int out;
    ()
