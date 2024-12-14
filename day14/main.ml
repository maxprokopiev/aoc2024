let input = Utils.Files.read_file "day14/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)

let re =
        Re.Posix.compile (Re.Posix.re "p=([0-9]+),([0-9]+) v=(-?[0-9]+),(-?[0-9]+)")

let robots = Hashtbl.create 666

let prepare_input input =
  input
  |> chomp
  |> Re.Str.split (Re.Str.regexp "\n")
  |> List.map (fun l -> Array.map (int_of_string) (Array.of_list (List.tl (Array.to_list (Re.Group.all (Re.exec re l))))))
  |> Array.of_list
  |> Array.iter (fun a ->
                  if Hashtbl.mem robots (a.(1), a.(0)) then
                          let vs = Hashtbl.find robots (a.(1), a.(0)) in
                          Hashtbl.replace robots (a.(1), a.(0)) (vs @ [(a.(3), a.(2))])
                  else
                          Hashtbl.add robots (a.(1), a.(0)) [(a.(3), a.(2))])

(*
let gr = 7 (* 103 *)
let gc = 11 (* 101 *)
*)


let gr = 103
let gc = 101

let hash_append h k v =
        if Hashtbl.mem h k then
                let vs = Hashtbl.find h k in
                Hashtbl.replace h k (vs @ [v])
        else
                Hashtbl.add h k [v]

let move r c dr dc =
        let nr' = (r + dr) in
        let nc' = (c + dc) in
        let nr = if nr' < 0 then nr' + gr else if nr' >= gr then nr' - gr else nr' in
        let nc = if nc' < 0 then nc' + gc else if nc' >= gc then nc' - gc else nc' in
        (*let _ = Printf.printf "move (%d, %d) by (%d, %d) is (%d, %d) -> (%d, %d)\n" r c dr dc nr' nc' nr nc in*)
        (nr, nc)

let print_robots oc =
        for r = 0 to gr - 1 do
                for c = 0 to gc - 1 do
                        Printf.fprintf oc "%s" (if Hashtbl.mem robots (r, c) then (string_of_int (List.length (Hashtbl.find robots (r, c)))) else ".");
                done;
                Printf.fprintf oc "\n";
        done

let write_output oc t =
        let _ = Printf.fprintf oc "%d\n" t in
        let _ = print_robots oc in
        let _ = Printf.fprintf oc "\n----------------\n" in
        ()

let solve1 =
        let oc = open_out "day14/out.txt" in
        let _ = prepare_input input in
        for t = 0 to 100000 do
                let sum1 = ref 0 in
                let robots' = Hashtbl.to_seq robots |> List.of_seq in
                if t = 7371 then (write_output oc t);

                Hashtbl.clear robots;
                List.iter (fun (coords, vs) ->
                        let r = fst coords in
                        let c = snd coords in
                        if c >= 45 then sum1 := !sum1 + 1;
                        if c <= 75 then sum1 := !sum1 + 1;
                        List.iter (fun (dr, dc) -> hash_append robots (move r c dr dc) (dr, dc)) vs
                ) robots';
                (*if !sum1 > 880 then Printf.printf "t=%d sum1=%d\n" t !sum1;*)
                (*if !sum1 = 885 then (write_output oc t);*)
        done;
        close_out oc;
        let qs = List.fold_left (fun (a1, a2, a3, a4) (coords, vs) ->
                let r = fst coords in
                let c = snd coords in
                if r < (gr / 2) && c < (gc / 2) then
                        (a1 + List.length vs, a2, a3, a4)
                else if r < (gr / 2) && c > (gc / 2) then
                        (a1, a2 + List.length vs, a3, a4)
                else if r > (gr / 2) && c < (gc / 2) then
                        (a1, a2, a3 + List.length vs, a4)
                else if r > (gr / 2) && c > (gc / 2) then
                        (a1, a2, a3, a4 + List.length vs)
                else
                        (a1, a2, a3, a4)
        ) (0, 0, 0, 0) (Hashtbl.to_seq robots |> List.of_seq) in
        let (a1, a2, a3, a4) = qs in
        a1 * a2 * a3 * a4

let solve2 = 0

let () = Printf.printf "part1: %d | part2: %d\n%!" solve1 solve2;

