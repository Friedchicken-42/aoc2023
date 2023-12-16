include Core

type direction = Right | Left | Up | Down

type position = {
    x: int;
    y: int;
    dir: direction;
}

let next { x; y; dir } = match dir with
| Right -> { x = x + 1; y; dir }
| Left -> { x = x - 1; y; dir }
| Up -> { x; y = y - 1; dir }
| Down -> { x; y = y + 1; dir }

let move item d = next { item with dir = d }

let iter matrix visited item =
    let { x; y; dir } = item in
    let size = Array.length matrix in
    if x < 0 || x >= size || y < 0 || y >= size then 
        []
    else if visited.(y).(x) <= size * 5 then (
        (* why size * 5 works but not size? *)
        visited.(y).(x) <- visited.(y).(x) + 1;
        match (matrix.(y).(x), dir) with
        | ('.', _) -> [ next item ]
        | ('/', Right) -> [ move item Up ]
        | ('/', Left) -> [ move item Down ]
        | ('/', Up) -> [ move item Right ]
        | ('/', Down) -> [ move item Left ]
        | ('\\', Right) -> [ move item Down ] 
        | ('\\', Left) -> [ move item Up ]
        | ('\\', Up) -> [ move item Left ]
        | ('\\', Down) -> [ move item Right ]
        | ('-', (Up | Down)) -> [ move item Right; move item Left ]
        | ('-', _) -> [ next item ]
        | ('|', (Right | Left)) -> [ move item Down; move item Up ]
        | ('|', _) -> [ next item ]
        | (c, _) -> Printf.ksprintf failwith "Wrong Char %c" c
    ) else
        []

let beam matrix visited item = 
    let queue = Queue.create () in
    Queue.add item queue;

    let rec process queue =
        match Queue.take_opt queue with
        | None -> ()
        | Some item -> 
                let out = iter matrix visited item in
                List.iter (fun i -> Queue.add i queue) out;
                process queue;
            ()
    in

    process queue;
    ()

let border matrix =
    let size = Array.length matrix in
    List.init size (fun x -> { x; y = 0; dir = Down } ) @
    List.init size (fun x -> { x; y = size - 1; dir = Up } ) @
    List.init size (fun y -> { x = 0; y; dir = Right } ) @
    List.init size (fun y -> { x = size - 1; y; dir = Left } )

let () = 
    let lines = read_lines () in
    let lines = List.map (fun line -> Array.of_list (chars line)) lines in
    let matrix = Array.of_list lines in
    let size = Array.length matrix in

    let points = border matrix in

    let energized = List.map (fun item ->
        let visited = Array.make_matrix size size 0 in
        beam matrix visited item;
        let out = Array.fold_left (fun accs row ->
            Array.fold_left (fun acc i -> acc + if i != 0 then 1 else 0) accs row
        ) 0 visited in 
        out
    ) points in

    (* List.iter (Printf.printf "%5d\n") energized; *)
    let out = List.fold_left (max) 0 energized in
    print_int out;
    ()
