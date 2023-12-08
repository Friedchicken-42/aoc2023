include Core

type number = {
    x: int;
    y: int;
    size: int;
    value: int;
}

type symbol = {
    x: int;
    y: int;
    _value: char;
}

let chars s = List.of_seq (String.to_seq s)

let is_digit c =
    let value = Char.code c in
    value >= 48 && value <= 57

let number_init value x y = 
    let size = String.length (string_of_int value) in
    { 
        x = x - size;
        y; size; value;
    }

let rec parse_number lst y index value = match (lst, value) with
| (x :: xs, value) when is_digit x ->  
        let value = match value with
        | None -> 0
        | Some v -> v in
        parse_number xs y (index + 1) ( Some (value * 10 + (Char.code x - 48)))
| (_ :: xs, Some v) -> (number_init v index y) :: parse_number xs y (index + 1) None
| (_ :: xs, None) -> parse_number xs y (index + 1) value 
| ([], Some v) -> [ number_init v index y ]
| ([], None) -> []

let rec parse_symbols lst y x = match lst with
| c :: cs when not (is_digit c) && c != '.' -> 
        let s = {
            x; y;
            _value = c;
        } in
        s :: parse_symbols cs y (x + 1)
| _ :: cs -> parse_symbols cs y (x + 1)
| [] -> []

let adjacent numbers symbols = 
    let outer (symbol: symbol) = 
        let inner (number: number) = 
            symbol.x >= number.x - 1 && 
            symbol.x <= number.x + number.size &&
            symbol.y >= number.y - 1 &&
            symbol.y <= number.y + 1
        in

        let l = List.filter inner numbers in
        if List.length l == 2 then
            List.fold_left (fun acc n -> acc * n.value) 1 l
        else
            0
    in

    List.filter (fun x -> x != 0) (List.map outer symbols)

let () = 
    let lines = read_lines () in

    let numbers = List.mapi (fun i line -> 
        let c = chars line in
        parse_number c i 0 None
    ) lines in
    let numbers = List.flatten numbers in

    let symbols = List.mapi (fun i line ->
        let c = chars line in
        parse_symbols c i 0
    ) lines in
    let symbols = List.flatten symbols in

    let schematic = adjacent numbers symbols in
    let out = List.fold_left (fun acc value -> acc + value) 0 schematic in
    print_int  out;
    ()

