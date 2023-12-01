let read_lines () =
    let rec read_lines acc =
        try
            let line = input_line stdin in
            read_lines (line :: acc)
        with
            End_of_file -> List.rev acc
    in
    read_lines []
