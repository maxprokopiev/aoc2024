let input = Utils.Files.read_file "day10/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let lines = String.split_on_char '\n'

let prepare_input input =
  input
  |> chomp
  |> lines
  |> List.map (Re.Str.split (Re.Str.regexp ""))
  |> List.map (fun l -> List.map (int_of_string) l)
  |> List.map Array.of_list
  |> Array.of_list

let grid = prepare_input input
let gr = Array.length grid
let gc = Array.length grid.(0)

let dirs = [| (0, 1); (1, 0); (0, -1); (-1, 0); |]

let walk grid all (r, c) q =
        let sum = ref [] in
        Queue.push (r, c) q;
        while not (Queue.is_empty q) do
                let (r, c) = Queue.pop q in
                Array.iter (fun (dr, dc) ->
                        let nr = r + dr in
                        let nc = c + dc in
                        if nr >= 0 && nr < gr && nc >= 0 && nc < gc && (grid.(nr).(nc) - grid.(r).(c) = 1) then
                                        begin
                                                if grid.(nr).(nc) = 9 then
                                                        sum := (nr, nc) :: !sum
                                                else
                                                        Queue.push (nr, nc) q
                                        end
                ) dirs
        done;
        if all then List.length !sum else List.length (Utils.Lists.unique !sum)

let solve all =
        let sum = ref 0 in
        for r = 0 to gr - 1 do
                for c = 0 to gc - 1 do
                        if grid.(r).(c) = 0 then
                                sum := !sum + (walk grid all (r, c) (Queue.create ()))
                done
        done;
        !sum

let () = Printf.printf "part1: %d | part2: %d\n%!" (solve false) (solve true);

