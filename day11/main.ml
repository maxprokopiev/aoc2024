let input = Utils.Files.read_file "day11/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let lines = String.split_on_char '\n'

let prepare_input input =
  input
  |> chomp
  |> lines
  |> List.map (Re.Str.split (Re.Str.regexp " "))
  |> List.hd

let mem = Hashtbl.create 666

let rec blink t stone =
        if Hashtbl.mem mem (t, stone) then
                Hashtbl.find mem (t, stone)
        else
          if t = 0 then
                  1
          else
            let i = int_of_string stone in
            let res = (if i = 0 then
                      blink (t - 1) "1"
              else if String.length stone mod 2 = 0 then
                      let stones = [String.sub stone 0 ((String.length stone) / 2); String.sub stone ((String.length stone) / 2) (String.length stone / 2)] in
                      let ss = List.map (string_of_int) (List.map (int_of_string) stones) in
                      (blink (t - 1) (List.hd ss)) + (blink (t - 1) (List.hd (List.tl ss)))
              else
                      blink (t - 1) (string_of_int (i * 2024))) in
            Hashtbl.add mem (t, stone) res;
            res

let solve steps =
        input
        |> prepare_input
        |> List.map (blink steps)
        |> List.fold_left (+) 0

let () = Printf.printf "part1: %d | part2: %d\n%!" (solve 25) (solve 75);

