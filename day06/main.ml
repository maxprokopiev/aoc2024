let input = Utils.Files.read_file "day06/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let lines = String.split_on_char '\n'

let prepare_input input =
  input
  |> chomp
  |> lines
  |> List.map (Re.Str.split (Re.Str.regexp ""))
  |> List.map Array.of_list
  |> Array.of_list

module IntTuple = struct
  type t = int * int * int
  let compare (x0, y0, z0) (x1, y1, z1) =
        if x0 = x1 && y0 = y1 && z0 = z1 then 0
        else
                if x0 < x1 then -1
                else if x0 > x1 then 1
                else
                if y0 < y1 then -1
                else if y0 > y1 then 1
                else
                if z0 < z1 then -1
                else 1
end

module IntTupleSet = Set.Make(IntTuple)

let dirs = [| (-1, 0); (0, 1); (1, 0); (0, -1) |]

let add_if_exists list el =
        if List.mem el list then list else List.append list [el]

let rec find_index grid r c dir pos =
  let (dr, dc) = dirs.(dir) in
  let nr = r + dr in
  let nc = c + dc in
  if nr < 0 || nr >= Array.length grid || nc < 0 || nc >= Array.length grid.(0) then
    add_if_exists pos (r, c)
  else if grid.(nr).(nc) = "#" then
    find_index grid r c ((dir + 1) mod 4) (add_if_exists pos (r, c))
  else
    find_index grid nr nc dir (add_if_exists pos (r, c))

let solve1 sr sc =
        let grid = prepare_input input in
        List.length (find_index grid sr sc 0 [(sr, sc)])

let rec is_loop grid r c dir obr obc pos =
  if IntTupleSet.(pos |> mem (r, c, dir)) then
    true
  else
    let (dr, dc) = dirs.(dir) in
    let nr = r + dr in
    let nc = c + dc in
    if nr < 0 || nr >= Array.length grid || nc < 0 || nc >= Array.length grid.(0) then
              false
    else if grid.(nr).(nc) = "#" || (nr = obr && nc = obc) then
            is_loop grid r c ((dir + 1) mod 4) obr obc IntTupleSet.(pos |> add (r, c, dir))
    else
            is_loop grid nr nc dir obr obc IntTupleSet.(pos |> add (r, c, dir))

let check_pos grid sr sc r c =
        if grid.(r).(c) = "." then if is_loop grid sr sc 0 r c IntTupleSet.empty then true else false else false

let solve2 sr sc =
        let grid = prepare_input input in
        let positions = find_index grid sr sc 0 [(sr, sc)] in
        let count = ref 0 in
        List.iter (fun pos -> if (check_pos grid sr sc (fst pos) (snd pos)) then count := !count + 1) positions;!count

let () = Printf.printf "part1: %d | part2: %d\n%!" (solve1 58 80) (solve2 58 80);
(*let () = Printf.printf "part1: %d | part2: %d\n%!" (solve1 6 4) (solve2 6 4);*)

