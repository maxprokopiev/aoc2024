let input = Utils.Files.read_file "day13/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)

let re =
        Re.Posix.compile (Re.Posix.re "Button A: X\\+([0-9]+), Y\\+([0-9]+)\nButton B: X\\+([0-9]+), Y\\+([0-9]+)\nPrize: X=([0-9]+), Y=([0-9]+)")

let prepare_input input =
  input
  |> chomp
  |> Re.Str.split (Re.Str.regexp "\n\n")
  |> List.map (fun l -> List.map (int_of_string) (List.tl (Array.to_list (Re.Group.all (Re.exec re l)))))

let solve' n args =
        match args with
        | [] -> None
        | a11::a21::a12::a22::b1'::b2'::[] ->
          let b1 = b1' + n in
          let b2 = b2' + n in
          let d = a11 * a22 - a12 * a21 in
          let dx = b1 * a22 - b2 * a12 in
          let dy = a11 * b2 - a21 * b1 in
          if d = 0 then
                  None
          else
                  let dxd = dx / d in
                  let dyd = dy / d in
                  if dxd * a11 + dyd * a12 = b1 && dxd * a21 + dyd * a22 = b2 then
                          Some (dxd, dyd)
                  else
                          None
        | _ -> None

let solve n =
  input
  |> prepare_input
  |> List.map (solve' n)
  |> List.filter_map (fun x -> x)
  |> List.fold_left (fun acc (a, b) -> acc + ((a * 3) + b)) 0

let () = Printf.printf "part1: %d | part2: %d\n%!" (solve 0) (solve 10000000000000);

