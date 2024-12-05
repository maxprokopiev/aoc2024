let input = Utils.Files.read_file "day05/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let pair = Re.Str.split (Re.Str.regexp "\n\n") (chomp input)
let rules_raw = List.hd pair
let pages =
        List.hd (List.tl pair)
        |> Re.Str.split (Re.Str.regexp "\n")
        |> List.map (Re.Str.split (Re.Str.regexp ","))
        |> List.map (fun p -> List.map int_of_string p)

let rules = Hashtbl.create 666

let build_rules =
        rules_raw
        |> Re.Str.split (Re.Str.regexp "\n")
        |> List.map (fun r -> List.map int_of_string (Re.Str.split (Re.Str.regexp "|") r))
        |> List.map (fun r -> (Hashtbl.add rules (List.hd r) (List.hd (List.tl r))); r)

let compare x y = if (List.mem y (Hashtbl.find_all rules x)) then -1 else 1

let rec is_sorted xs = match xs with
  | [] -> true
  | _::[] -> true
  | x::y::z -> if (compare x y) = -1 then is_sorted (y::z) else false

let solve1 =
        let _ = build_rules in
        pages
        |> List.filter (is_sorted)
        |> List.map (fun l -> List.nth l ((List.length l) / 2))
        |> List.fold_left (+) 0

let solve2 =
        pages
        |> List.filter (fun l -> not (is_sorted l))
        |> List.map (List.sort compare)
        |> List.map (fun l -> List.nth l ((List.length l) / 2))
        |> List.fold_left (+) 0

let () = Printf.printf "part1: %d | part2: %d\n%!" solve1 solve2;

