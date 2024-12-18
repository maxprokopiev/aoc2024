let input = Utils.Files.read_file "day18/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let lines = String.split_on_char '\n'

let prepare_input input =
  input
  |> chomp
  |> lines
  |> List.map (fun line ->
                  let bytes = List.map (int_of_string) (String.split_on_char ',' line) in
                  (List.hd bytes, List.hd (List.tl bytes)))

module IntTuple = struct
  type t = int * int * int * int
  let compare (x0, y0, z0, a0) (x1, y1, z1, a1) =
        if x0 = x1 && y0 = y1 && z0 = z1 && a0 = a1 then 0
        else
                if x0 < x1 then -1
                else if x0 > x1 then 1
                else
                if y0 < y1 then -1
                else if y0 > y1 then 1
                else
                if z0 < z1 then -1
                else
                if a0 < a1 then -1
                else 1
end
module IntTupleSet = Set.Make(IntTuple)

let add_to_set set el =
        set := IntTupleSet.add el !set

module Heap = Pairing_heap

let dirs = [| (0, 1); (1, 0); (0, -1); (-1, 0); |]

let min list = List.fold_left Int.min (List.hd list) list
let compare (_,_,_,_,n) (_,_,_,_,m) = Int.compare n m

let get_paths bytes gr gc =
          let been = ref IntTupleSet.empty in
          let sums = ref [] in
          let start = (0, 0) in
          let finish = (gr - 1, gc - 1) in
          let q = Heap.create ~cmp:compare () in
          let _ = Array.iter (fun (dr, dc) ->
                  Heap.add q ((fst start), (snd start), dr, dc, 0)
          ) dirs in
          while not (Heap.is_empty q) do
                  let r, c, dirr, dirc, sum = Heap.pop_exn q in
                  let _ = if (r, c) = finish then begin
                          let _ = sums := sum :: !sums in () end in
                  if not (IntTupleSet.mem (r, c, dirr, dirc) !been) then
                          let _ = add_to_set been (r, c, dirr, dirc) in
                          Array.iter (fun (dr, dc) ->
                                  let nr = r + dirr in
                                  let nc = c + dirc in
                                  if (nr >= 0) && (nr < gr) && (nc >= 0) && (nc < gc) && (not (Hashtbl.mem bytes (nr, nc))) then
                                          Heap.add q (nr, nc, dr, dc, sum + 1)
                          ) dirs
          done;
          !sums


let solve1 =
        let bytes = Hashtbl.create 666 in
        let all_bytes = prepare_input input in
        let gr = 71 in
        let gc = 71 in
        for i = 0 to 1023 do
          let byte = List.nth all_bytes i in
          let _ = Hashtbl.add bytes byte "#" in ()
        done;
        min (get_paths bytes gr gc)

let solve2 =
        let bytes = Hashtbl.create 666 in
        let all_bytes = prepare_input input in
        let gr = 71 in
        let gc = 71 in
        let coords = ref [] in
        for i = 0 to (List.length all_bytes) - 1 do
          let byte = List.nth all_bytes i in
          let _ = Hashtbl.add bytes byte "#" in
          if ((List.length (get_paths bytes gr gc)) = 0) then
                  coords := !coords @ [byte];
        done;
        List.hd !coords

let () = Printf.printf "part1: %d | part2: (%d, %d)\n%!" solve1 (fst solve2) (snd solve2);
