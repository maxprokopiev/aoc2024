let input = Utils.Files.read_file "day03/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let lines = String.split_on_char '\n'

let prepare_input input =
  input
  |> chomp
  |> lines

let get_ops str = Re.matches (Re.Posix.compile (Re.Posix.re "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")) str

let get_args op =
        op
        |> Re.exec (Re.Posix.compile (Re.Posix.re "\\(([0-9]{1,3}),([0-9]{1,3})\\)"))
        |> Re.Group.all
        |> Array.to_list
        |> List.tl
        |> List.map int_of_string

let calc ops = List.fold_left ( + ) 0  (List.map (fun x -> List.fold_left ( * ) 1 (get_args x)) ops)

let filter_donts s = List.filter (fun s -> not (String.starts_with ~prefix:"n't()" s)) (Re.Str.split (Re.Str.regexp "do") s)

let solve1 =
  input
  |> prepare_input
  |> String.concat ""
  |> get_ops
  |> calc

let solve2 =
  input
  |> prepare_input
  |> String.concat ""
  |> filter_donts
  |> List.map (fun s -> calc (get_ops s))
  |> List.fold_left ( + ) 0

let () = Printf.printf "part1: %d | part2: %d\n%!" solve1 solve2;

