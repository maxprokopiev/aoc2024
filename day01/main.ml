let input = Utils.Files.read_file "day01/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let lines = String.split_on_char '\n'

let prepare_input input =
  input
  |> chomp
  |> lines
  |> List.map (Str.split (Str.regexp " +"))
  |> List.map (List.map int_of_string)

let rec transpose list = match list with
| []             -> []
| []   :: xss    -> transpose xss
| (x::xs) :: xss ->
    (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let compare a b = if a < b then -1 else if a > b then 1 else 0

let sort l = [(List.sort compare (List.hd l)); (List.sort compare (List.nth l 1))]

let count_occur x list = List.length (List.filter (fun e -> x = e) list)

let similarity_score (l1, l2) =
        List.fold_left (fun acc el -> acc + el * (count_occur el l2)) 0 l1

let to_tuple l = (List.hd l, List.nth l 1)

let solve2 =
  input
  |> prepare_input
  |> transpose
  |> to_tuple
  |> similarity_score

let solve1 =
  input
  |> prepare_input
  |> transpose
  |> sort
  |> transpose
  |> List.map (fun l -> Int.abs (List.hd l - List.nth l 1))
  |> List.fold_left (+) 0

let () = Printf.printf "part1: %d | part2: %d\n%!" solve1 solve2
