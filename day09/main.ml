let input = Utils.Files.read_file "day09/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let lines = String.split_on_char '\n'

let prepare_input input =
  input
  |> chomp
  |> lines
  |> List.hd
  |> Re.Str.split (Re.Str.regexp "")
  |> List.map (int_of_string)
  |> Array.of_list

let range from until =
  List.init (until - from) (fun i -> i + from)

let checksum i id nb =
        List.fold_left (fun acc x -> acc + (x * id)) 0 (range i (i + nb))

let rec compact s i bi ir lid lnb rid rnb sum =
        if i > bi then
                sum
        else
          if i = 0 || (i mod 2 = 0) then
                  let new_lnb = if i = bi then rnb else lnb in
                  let checksum = checksum ir lid new_lnb in
                  compact s (i + 1) bi (ir + lnb) (lid + 1) s.(i + 1) rid rnb (sum + checksum)
          else
                  let nb, ni, nbi, new_lid, new_lnb, new_rid, new_rnb =
                          if rnb > lnb then
                                  lnb, i + 1, bi, lid, s.(i+1), rid, rnb - lnb
                          else
                                  rnb, i, bi - 2, lid, lnb - rnb, rid - 1, s.(bi - 2) in
                  let checksum = checksum ir rid nb in
                  compact s ni nbi (ir + nb) new_lid new_lnb new_rid new_rnb (sum + checksum)

let solve1 =
        let s = prepare_input input in
        let sl = Array.length s in
        compact s 0 (sl - 1) 0 0 s.(0) (sl / 2) s.(sl - 1) 0

let split (i, files, spaces) (ix, x) =
        if ix mod 2 = 0 then
                (i + x, Array.append files [|(i, x)|], spaces)
        else
                (i + x, files, Array.append spaces [|(i, x)|])

let reduce (sum, spaces) (id, (i, nbl)) =
        let res = Array.find_mapi (fun ind (ix, nb) ->
                if ix < i && nb >= nbl then
                        Some (ind, ix, nbl)
                else
                        None) spaces in
        match res with
        | Some (ind, ix, nb) ->
                        (sum + checksum ix id nb, Array.mapi (fun j (ni, nbs) -> if j = ind then (ni + nbl, nbs - nbl) else (ni, nbs)) spaces)
        | None -> (sum + checksum i id nbl, spaces)

let solve2 =
        let s = prepare_input input in
        let _, files, spaces = Array.fold_left split (0, [||], [||]) (Array.mapi (fun i x -> (i, x)) s) in
        let spaces = Array.of_list (List.filter (fun (_, nb) -> not (nb = 0)) (Array.to_list spaces)) in
        let files = Array.of_list (List.rev (List.tl (Array.to_list files))) in
        let ids = Array.of_list (List.rev (range 1 ((Array.length files) + 1))) in
        let sum, _ = Array.fold_left reduce (0, spaces) (Array.map2 (fun i el -> (i, el)) ids files) in
        sum

let () = Printf.printf "part1: %d | part2: %d\n%!" solve1 solve2;

