let input = Utils.Files.read_file "day07/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let lines = String.split_on_char '\n'

let prepare_input input =
  input
  |> chomp
  |> lines
  |> List.map (fun l ->
                  let parts = Re.Str.split (Re.Str.regexp ":") in
                  let res = int_of_string (List.hd (parts l)) in
                  let args = List.map (int_of_string) (Re.Str.split (Re.Str.regexp " ") (List.hd (List.tl (parts l)))) in
                  res :: args)

let rec check x xs r =
        if List.length xs = 0 then
            x = r
        else
        let plus = x + (List.hd xs) in
        let mult = x * (List.hd xs) in
        (check plus (List.tl xs) r) || (check mult (List.tl xs) r)

let rec check_more x xs r =
        if List.length xs = 0 then
            x = r
        else
        let plus = x + (List.hd xs) in
        let mult = x * (List.hd xs) in
        let concat = (String.concat "" [(string_of_int x); (string_of_int (List.hd xs))]) in
        (check_more plus (List.tl xs) r) || (check_more mult (List.tl xs) r) || (check_more (int_of_string concat) (List.tl xs) r)

let solve1 =
  input
  |> prepare_input
  |> List.filter (fun l ->
                  let res = List.hd l in
                  let args = List.tl l in
                  check (List.hd args) (List.tl args) res)
  |> List.fold_left (fun acc l -> acc + (List.hd l)) 0

let solve2 =
  input
  |> prepare_input
  |> List.filter (fun l ->
                  let res = List.hd l in
                  let args = List.tl l in
                  check_more (List.hd args) (List.tl args) res)
  |> List.fold_left (fun acc l -> acc + (List.hd l)) 0

let () = Printf.printf "part1: %d | part2: %d\n%!" solve1 solve2;
