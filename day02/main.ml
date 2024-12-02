let input = Utils.Files.read_file "day02/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let lines = String.split_on_char '\n'

let prepare_input input =
  input
  |> chomp
  |> lines
  |> List.map (String.split_on_char ' ')
  |> List.map (List.map int_of_string)

let find_differences list =
        let l1 = list in
        let l2 = List.tl (List.append list [0]) in
        List.map2 (-) l1 l2
        |> List.filteri (fun i _ -> i < (List.length list) - 1)

let is_decreasing list =
        List.for_all (fun x -> x = -1 || x = -2 || x = -3) list

let is_increasing list =
        List.for_all (fun x -> x = 1 || x = 2 || x = 3) list

let is_safe list =
        is_decreasing list || is_increasing list

let solve1 =
  input
  |> prepare_input
  |> List.map (find_differences)
  |> List.filter (is_safe)
  |> List.length

let build_possible_lists list =
        List.append (List.mapi (fun i _ -> (List.filteri (fun ii _ -> ii != i) list)) list) [list]

let is_really_safe list =
        List.exists (is_safe) (List.map (find_differences) (build_possible_lists list))

let solve2 =
  input
  |> prepare_input
  |> List.filter (fun l -> is_safe l || is_really_safe l)
  |> List.length

let () = Printf.printf "part1: %d | part2: %d\n%!" solve1 solve2;
