let input1 = Utils.Files.read_file "day15/input.txt"
let input2 = Utils.Files.read_file "day15/input2.txt"
let chomp str = String.sub str 0 ((String.length str)-1)
let pair str =
        let l = Re.Str.split (Re.Str.regexp "\n\n") (chomp str) in
        (List.hd l, List.hd (List.tl l))

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

let add_to_set set el =
        set := IntTupleSet.add el !set

let dirs =
        let h = Hashtbl.create 4 in
        let _ = Hashtbl.add h ">" (0,1) in
        let _ = Hashtbl.add h "<" (0,-1) in
        let _ = Hashtbl.add h "v" (1,0) in
        let _ = Hashtbl.add h "^" (-1,0) in
        h

let find_spot coords pos r c dr dc =
        let stop = ref false in
        let found = ref false in
        let nr, nc = ref (r - dr), ref (c - dc) in
        while (not !stop) do
                nr := !nr + dr;
                nc := !nc + dc;
                if not (Hashtbl.mem coords (!nr, !nc)) then
                        begin
                          found := true;
                          stop := true;
                        end
                else if (Hashtbl.mem coords (!nr, !nc)) && (Hashtbl.find coords (!nr, !nc) = "#") then
                        stop := true
        done;
        if !found then
                begin
                        Hashtbl.remove coords !pos;
                        Hashtbl.replace coords (r, c) "@";
                        Hashtbl.replace coords (!nr, !nc) "O";
                        pos := (r, c);
                end

let try_moving coords pos r c dr dc =
        let q = Queue.create () in
        Queue.push (r - dr, c - dc) q;
        let been = ref IntTupleSet.empty in
        let can_move = ref true in
        while (not (Queue.is_empty q)) && !can_move do
                let (r', c') = Queue.pop q in
                if not (IntTupleSet.mem (r', c') !been) then
                        begin
                                add_to_set been (r', c');
                                let nr = r' + dr in
                                let nc = c' + dc in
                                if Hashtbl.mem coords (nr, nc) then
                                        begin
                                                if Hashtbl.find coords (nr, nc) = "#" then
                                                        can_move := false
                                                else if Hashtbl.find coords (nr, nc) = "[" then
                                                        begin
                                                                Queue.push (nr, nc) q;
                                                                Queue.push (nr, nc + 1) q;
                                                        end
                                                else if Hashtbl.find coords (nr, nc) = "]" then
                                                        begin
                                                                Queue.push (nr, nc) q;
                                                                Queue.push (nr, nc - 1) q;
                                                        end
                                        end
                        end
        done;
        if !can_move then
                begin
                        while (not (IntTupleSet.is_empty !been)) do
                                Seq.iter (fun (rr, cc) ->
                                        let nr, nc = rr + dr, cc + dc in
                                        if (not (IntTupleSet.mem (nr, nc) !been)) then
                                                begin
                                                        Hashtbl.replace coords (nr, nc) (Hashtbl.find coords (rr, cc));
                                                        Hashtbl.remove coords (rr, cc);
                                                        been := IntTupleSet.remove (rr, cc) !been
                                                end
                                ) (IntTupleSet.to_seq !been)
                        done;
                        Hashtbl.remove coords !pos;
                        pos := (r, c)
                end

let solve input chr fn =
        let coords = Hashtbl.create 666 in
        let (grid'', moves') = pair input in
        let moves'' = moves' |> Re.Str.split (Re.Str.regexp "\n") in
        let moves = Re.Str.split (Re.Str.regexp "") (List.fold_left (^) "" moves'') in
        let grid' = Re.Str.split (Re.Str.regexp "\n") grid'' in
        let grid = List.map (fun x -> Re.Str.split (Re.Str.regexp "") x) grid' in
        let pos = ref (0,0) in
        let _ = List.iteri (fun r row ->
                List.iteri (fun c el ->
                        let _ = if not (el = ".") then Hashtbl.add coords (r,c) el in
                        if el = "@" then pos := (r,c)
                        ) row
                ) grid in
        List.iter (fun dir ->
                let (dr, dc) = Hashtbl.find dirs dir in
                let (nr, nc) = ((fst !pos) + dr, (snd !pos) + dc) in
                if Hashtbl.mem coords (nr, nc) then
                        begin
                                if (Hashtbl.find coords (nr, nc)) <> "#" then (fn coords pos nr nc dr dc)
                        end
                else
                        begin
                                Hashtbl.remove coords !pos;
                                Hashtbl.replace coords (nr, nc) "@";
                                pos := (nr, nc);
                        end
        ) moves;
        Hashtbl.fold (fun (r, c) v sum -> if v = chr then sum + (100 * r) + c else sum) coords 0

let () = Printf.printf "part1: %d | part2: %d\n%!" (solve input1 "O" find_spot) (solve input2 "[" try_moving);

