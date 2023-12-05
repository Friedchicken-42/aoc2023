include Core

let print_int_newline n = print_int n; print_newline ()

let parse_seeds lines = 
    let (line, _blank, other) = match lines with
    | a :: b :: c -> (a, b, c)
    | _ -> failwith "Wrong match" in

    let seeds = match Str.split (Str.regexp " ") line with
    | (_ :: xs) -> xs
    | [] -> failwith "Wrong match" in

    (List.map int_of_string seeds, other)

type category = None | STS | STF | FTW | WTL | LTT | TTU | UTL

type range = {
    destination: int;
    source: int;
    size: int;
}

type almanac = {
    seed_to_soil: range list;
    soil_to_fertilizer: range list;
    fertilizer_to_water: range list;
    water_to_light: range list;
    light_to_temperature: range list;
    temperature_to_humidity: range list;
    humidity_to_location: range list;
}

let parse_range line = 
    let data = List.map int_of_string (Str.split (Str.regexp " ") line) in
    {
        destination = List.nth data 0;
        source = List.nth data 1;
        size = List.nth data 2;
    }

let rec parse acc category lines = match (category, lines) with
| (_, "" :: xs) -> parse acc None xs
| (STS, x :: xs) -> parse { acc with seed_to_soil = (acc.seed_to_soil @ [parse_range x]) } STS xs
| (STF, x :: xs) -> parse { acc with soil_to_fertilizer = (acc.soil_to_fertilizer @ [parse_range x]) } STF xs
| (FTW, x :: xs) -> parse { acc with fertilizer_to_water = (acc.fertilizer_to_water @ [parse_range x]) } FTW xs
| (WTL, x :: xs) -> parse { acc with water_to_light = (acc.water_to_light @ [parse_range x]) } WTL xs
| (LTT, x :: xs) -> parse { acc with light_to_temperature = (acc.light_to_temperature @ [parse_range x]) } LTT xs
| (TTU, x :: xs) -> parse { acc with temperature_to_humidity = (acc.temperature_to_humidity @ [parse_range x]) } TTU xs
| (UTL, x :: xs) -> parse { acc with humidity_to_location = (acc.humidity_to_location @ [parse_range x]) } UTL xs
| (_, "seed-to-soil map:" :: xs) -> parse acc STS xs
| (_, "soil-to-fertilizer map:" :: xs) -> parse acc STF xs
| (_, "fertilizer-to-water map:" :: xs) -> parse acc FTW xs
| (_, "water-to-light map:" :: xs) -> parse acc WTL xs
| (_, "light-to-temperature map:" :: xs) -> parse acc LTT xs
| (_, "temperature-to-humidity map:" :: xs) -> parse acc TTU xs
| (_, "humidity-to-location map:" :: xs) -> parse acc UTL xs
| _ -> acc

let contains ranges value =
    let a = List.filter (fun r -> r.source <= value && r.source + r.size > value) ranges in
    match a with
    | [r] -> Some r
    | _ -> None

let convert ranges value = 
    match contains ranges value with
    | Some r -> value - r.source + r.destination
    | None -> value

let stuff almanac seed =
    convert almanac.humidity_to_location
    (convert almanac.temperature_to_humidity
        (convert almanac.light_to_temperature
            (convert almanac.water_to_light
                (convert almanac.fertilizer_to_water
                    (convert almanac.soil_to_fertilizer
                        (convert almanac.seed_to_soil seed))))))

let rec find_min values = 
    match values with
    | [] -> failwith "empty list"
    | [x] -> x
    | x :: xs -> min x (find_min xs)

let () = 
    let lines = read_lines () in
    let (seeds, lines) = parse_seeds lines in

    let acc = {
        seed_to_soil = [];
        soil_to_fertilizer = [];
        fertilizer_to_water = [];
        water_to_light = [];
        light_to_temperature = [];
        temperature_to_humidity = [];
        humidity_to_location = [];
    } in

    let data = parse acc None lines in

    let out = List.map (stuff data) seeds in
    print_int_newline (find_min out);
    ()
