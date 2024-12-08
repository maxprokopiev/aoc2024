let input = Utils.Files.read_file "day08/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let lines = String.split_on_char '\n'

let prepare_input input =
  input
  |> chomp
  |> lines
  |> List.map (fun s -> Re.Str.split (Re.Str.regexp "") s)

let grid = prepare_input input
let gr = List.length grid
let gc = List.length (List.hd grid)

let is_valid a =
        (fst a) >= 0 && (fst a) < gr && (snd a) >= 0 && (snd a) < gc

let rec get_all_antinodes_for all op ant dr dc antinodes =
        let a = (op (fst ant) dr, op (snd ant) dc) in
        if is_valid a then
          if all then
            get_all_antinodes_for all op a dr dc antinodes @ [a]
          else
            [a]
        else
          if all then antinodes else []

let get_all_antinodes c1 c2 all =
        let dr = (fst c1) - (fst c2) in
        let dc = (snd c1) - (snd c2) in
        get_all_antinodes_for all (-) c2 dr dc [c2] @ get_all_antinodes_for all (+) c1 dr dc [c1]

let get_antinodes ants ant r c all =
        let new_coord = (r, c) in
        let ns = Hashtbl.find_all ants ant in
        Hashtbl.add ants ant new_coord;
        List.flatten (List.map (fun n -> get_all_antinodes new_coord n all ) ns)

let solve all =
  let ants = Hashtbl.create 666 in
  List.mapi (fun r row ->
    List.mapi (fun c el ->
        if not (el = ".") then get_antinodes ants el r c all else []
    ) row
  ) grid
  |> List.flatten |> List.flatten
  |> Utils.Lists.unique
  |> List.length

let () = Printf.printf "part1: %d | part2: %d\n%!" (solve false) (solve true);

