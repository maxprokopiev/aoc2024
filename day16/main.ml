let input = Utils.Files.read_file "day16/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)

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

let ardirs = [| (0, 1); (1, 0); (0, -1); (-1, 0); |]

let dirs =
        let d = Hashtbl.create 4 in
        let _ = Hashtbl.add d (-1, 0) [| (0, 1); (0, -1); |] in
        let _ = Hashtbl.add d (1, 0)  [| (0, 1); (0, -1); |] in
        let _ = Hashtbl.add d (0, 1)  [| (1, 0); (-1, 0); |] in
        let _ = Hashtbl.add d (0, -1) [| (1, 0); (-1, 0); |] in
        d

let opdirs =
        let d = Hashtbl.create 4 in
        let _ = Hashtbl.add d (-1, 0) (1, 0) in
        let _ = Hashtbl.add d (1, 0)  (-1, 0) in
        let _ = Hashtbl.add d (0, 1)  (0, -1) in
        let _ = Hashtbl.add d (0, -1) (0, 1) in
        d

let min list = List.fold_left Int.min (List.hd list) list
let compare (_,_,_,_,n) (_,_,_,_,m) = Int.compare n m

let dforward = Hashtbl.create 666
let dback = Hashtbl.create 666

let build_borders grid =
        let borders = Hashtbl.create 666 in
        List.iteri (fun r row -> List.iteri (fun c el -> if el = "#" then Hashtbl.add borders (r,c) el) row) grid;
        borders

let build_grid input =
        let grid = List.map (fun x -> Re.Str.split (Re.Str.regexp "") x) (Re.Str.split (Re.Str.regexp "\n") (chomp input)) in
        let gr = List.length grid in
        let gc = List.length (List.hd grid) in
        let start = (gr - 2, 1) in
        let finish = (1, gc - 2) in
        grid, gr, gc, start, finish

module Heap = Pairing_heap

let solve1 =
        let been = ref IntTupleSet.empty in
        let sums = ref [] in
        let grid, _, _, start, finish = build_grid input in
        let borders = build_borders grid in
        let q = Heap.create ~cmp:compare () in
        let _ = Heap.add q ((fst start), (snd start), 0, 1, 0) in
        while not (Heap.is_empty q) do
                let r, c, dirr, dirc, sum = Heap.pop_exn q in
                let _ = if not (Hashtbl.mem dforward (r, c, dirr, dirc)) then Hashtbl.add dforward (r, c, dirr, dirc) sum in
                let _ = if (r, c) = finish then begin
                        let _ = sums := sum :: !sums in () end in
                if not (IntTupleSet.mem (r, c, dirr, dirc) !been) then
                        let _ = add_to_set been (r, c, dirr, dirc) in
                        let nr = r + dirr in
                        let nc = c + dirc in
                        let _ = if (not (Hashtbl.mem borders (nr, nc))) then
                                let _ = Heap.add q (nr, nc, dirr, dirc, sum + 1) in () in
                        Array.iter (fun (dr, dc) ->
                                Heap.add q (r, c, dr, dc, sum + 1000)
                        ) (Hashtbl.find dirs (dirr, dirc))
        done;
        min !sums

let solve2 =
        let been = ref IntTupleSet.empty in
        let grid, gr, gc, _, finish = build_grid input in
        let borders = build_borders grid in
        let q = Heap.create ~cmp:compare () in
        let _ = Array.iter (fun (dr, dc) ->
                Heap.add q ((fst finish), (snd finish), dr, dc, 0)
        ) ardirs in
        while not (Heap.is_empty q) do
                let r, c, dirr, dirc, sum = Heap.pop_exn q in
                let _ = if not (Hashtbl.mem dback (r, c, dirr, dirc)) then Hashtbl.add dback (r, c, dirr, dirc) sum in
                if not (IntTupleSet.mem (r, c, dirr, dirc) !been) then
                        let _ = add_to_set been (r, c, dirr, dirc) in
                        let opdir = Hashtbl.find opdirs (dirr, dirc) in
                        let nr = r + (fst opdir) in
                        let nc = c + (snd opdir) in
                        let _ = if (not (Hashtbl.mem borders (nr, nc))) then
                                let _ = Heap.add q (nr, nc, dirr, dirc, sum + 1) in () in
                        Array.iter (fun (dr, dc) ->
                                Heap.add q (r, c, dr, dc, sum + 1000)
                        ) (Hashtbl.find dirs (dirr, dirc))
        done;
        let res = ref [] in
        for r = 0 to gr - 1 do
                for c = 0 to gc - 1 do
                        for d = 0 to 3 do
                                let dr = fst ardirs.(d) in
                                let dc = snd ardirs.(d) in
                                if Hashtbl.mem dforward (r, c, dr, dc) && Hashtbl.mem dback (r, c, dr, dc) then
                                        if (Hashtbl.find dforward (r, c, dr, dc)) + (Hashtbl.find dback (r, c, dr, dc)) = solve1 then
                                                res := (r, c) :: !res
                        done;
                done;
        done;
        List.length (Utils.Lists.unique !res)

let () = Printf.printf "part1: %d | part2: %d\n%!" solve1 solve2;

