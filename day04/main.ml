let input = Utils.Files.read_file "day04/sample.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let lines = String.split_on_char '\n'

let prepare_input input =
  input
  |> chomp
  |> lines
  |> List.map (fun s -> Re.Str.split (Re.Str.regexp "") s)

let rec transpose list = match list with
| []             -> []
| []   :: xss    -> transpose xss
| (x::xs) :: xss ->
    (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let get_matches re str = List.length (Re.matches (Re.Posix.compile (Re.Posix.re re)) str)

let count_matches lines =
  lines
  |> List.map (fun l -> (get_matches "XMAS" (String.concat "" l)) + (get_matches "SAMX" (String.concat "" l)))
  |> List.fold_left (+) 0

let nth_opt l i = match (List.nth_opt l i) with
         | None -> []
         | Some x -> [ x ]

let nth_row l i = match (List.nth_opt l i) with
         | None -> []
         | Some x -> x

let nth_el l i = match (List.nth_opt l i) with
         | None -> ""
         | Some x -> x

let get_diagonal x i xs =  List.fold_left (fun acc line -> ((List.append (fst acc) (nth_opt line ((snd acc) + 1))), (snd acc) + 1)) ([x],i) (List.tl xs)
let get_upper_diagonal lines = List.map (fst) (List.mapi (fun i x -> (get_diagonal x i lines)) (List.hd lines))

let check_mas lines r c =
        ((nth_el (nth_row lines r) c) = "M" &&
        (nth_el (nth_row lines r) (c + 2)) = "S" &&
        (nth_el (nth_row lines (r + 1)) (c + 1)) = "A" &&
        (nth_el (nth_row lines (r + 2)) c) = "M" &&
        (nth_el (nth_row lines (r + 2)) (c + 2)) = "S") ||

        ((nth_el (nth_row lines r) c) = "S" &&
        (nth_el (nth_row lines r) (c + 2)) = "S" &&
        (nth_el (nth_row lines (r + 1)) (c + 1)) = "A" &&
        (nth_el (nth_row lines (r + 2)) c) = "M" &&
        (nth_el (nth_row lines (r + 2)) (c + 2)) = "M") ||

        ((nth_el (nth_row lines r) c) = "M" &&
        (nth_el (nth_row lines r) (c + 2)) = "M" &&
        (nth_el (nth_row lines (r + 1)) (c + 1)) = "A" &&
        (nth_el (nth_row lines (r + 2)) c) = "S" &&
        (nth_el (nth_row lines (r + 2)) (c + 2)) = "S") ||

        ((nth_el (nth_row lines r) c) = "S" &&
        (nth_el (nth_row lines r) (c + 2)) = "M" &&
        (nth_el (nth_row lines (r + 1)) (c + 1)) = "A" &&
        (nth_el (nth_row lines (r + 2)) c) = "S" &&
        (nth_el (nth_row lines (r + 2)) (c + 2)) = "M")

let solve1 =
        let lines = prepare_input input in
        let m1 = count_matches lines in
        let m2 = count_matches (transpose lines) in
        let m3 = count_matches (get_upper_diagonal lines) in
        let m4 = count_matches (List.tl (get_upper_diagonal (transpose lines))) in
        let m5 = count_matches (get_upper_diagonal (List.map (List.rev) lines)) in
        let m6 = count_matches (List.tl (get_upper_diagonal (transpose (List.map (List.rev) lines)))) in
        m1 + m2 + m3 + m4 + m5 + m6

let solve2 =
        let lines = prepare_input input in
        List.fold_left (fun acc b -> if b then acc + 1 else acc) 0 (List.flatten (List.mapi (fun r l -> List.mapi (fun c _ -> check_mas lines r c) l) lines))

let () = Printf.printf "part1: %d | part2: %d\n%!" solve1 solve2;

