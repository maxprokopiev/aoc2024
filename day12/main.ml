let input = Utils.Files.read_file "day12/input.txt"
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
  type t = int * int
  let compare (x0, y0) (x1, y1) =
        if x0 = x1 && y0 = y1 then 0
        else
                if x0 < x1 then -1
                else if x0 > x1 then 1
                else
                if y0 < y1 then -1
                else 1
end

module IntTupleSet = Set.Make(IntTuple)

let grid = prepare_input input
let gr = Array.length grid
let gc = Array.length grid.(0)

let dirs = [| (0, 1); (1, 0); (0, -1); (-1, 0); |]
let been = ref IntTupleSet.empty

let add_to_set set el =
        set := IntTupleSet.add el !set

let find_plants grid r c =
        let plant = grid.(r).(c) in
        let rec find_plants' r' c' =
                if IntTupleSet.mem (r', c') !been then
                        []
                else
                  let _ = add_to_set been (r', c') in
                  Array.fold_left (fun acc (dr, dc) ->
                          let nr = r' + dr in
                          let nc = c' + dc in
                          if nr < 0 || nr >= gr || nc < 0 || nc >= gc then
                                  acc
                          else if grid.(nr).(nc) <> plant then
                                  acc
                          else
                                  acc @ (find_plants' nr nc)
                  ) [(r', c')] dirs
        in
        find_plants' r c

let perimeter plants sides = 
        Array.fold_left (fun acc (dr, dc) ->
                let fences = Utils.Lists.unique (List.filter_map (fun (r, c) ->
                        if (List.mem (r + dr, c + dc) plants) then
                                None
                        else
                                Some (r + dr, c + dc)

                ) plants) in
                if sides then
                        acc + (List.length (List.filter (fun (r, c) -> not (List.mem (r - dc, c - dr) fences)) fences))
                else
                        acc + (List.length fences)
        ) 0 dirs

let solve sides =
        been := IntTupleSet.empty;
        let sum = Hashtbl.create 666 in
        for r = 0 to gr - 1 do
                for c = 0 to gc - 1 do
                        if IntTupleSet.mem (r, c) !been then
                                ()
                        else
                                let plants = find_plants grid r c in
                                Hashtbl.add sum grid.(r).(c) (List.length plants, perimeter plants sides)
                done
        done;
        Hashtbl.fold (fun _ (a, p) acc -> acc + (a * p)) sum 0

let () = Printf.printf "part1: %d | part2: %d\n%!" (solve false) (solve true);

