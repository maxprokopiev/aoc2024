let input = Utils.Files.read_file "day19/input.txt"
let chomp str = String.sub str 0 ((String.length str)-1)

let prepare_input input =
        let pair = Re.Str.split (Re.Str.regexp "\n\n") (chomp input) in
        let towels, patterns = List.nth pair 0, List.nth pair 1 in
        let towels = Array.of_list (Re.Str.split (Re.Str.regexp ", ") towels) in
        let patterns = Re.Str.split (Re.Str.regexp "\n") patterns in
        (towels, patterns)

module StrIntTuple = struct
  type t = string * int
  let compare (x0, y0) (x1, y1) =
        if x0 = x1 && y0 = y1 then 0
        else
                if x0 < x1 then -1
                else if x0 > x1 then 1
                else
                if y0 < y1 then -1
                else 1
end

module StrIntTupleSet = Set.Make(StrIntTuple)

let is_possible pattern towels =
        let tried = ref StrIntTupleSet.empty in
        let p = ref "" in
        let pi = ref 0 in
        let ts = ref [] in
        while (!pi >= 0) && (!p <> pattern) do
                let ti = ref 0 in
                while (!ti < (Array.length towels)) && ((not (String.starts_with ~prefix:(!p ^ towels.(!ti)) pattern)) || (StrIntTupleSet.mem (towels.(!ti), !pi) !tried)) do
                        ti := !ti + 1
                done;
                if (!ti >= (Array.length towels)) || (StrIntTupleSet.mem (towels.(!ti), !pi) !tried) then
                        begin
                          pi := !pi - 1;
                          if String.length !p > 0 then
                                  begin
                                          let last = List.hd !ts in
                                          let ln = String.length last in
                                          p := String.sub !p 0 ((String.length !p) - ln);
                                          tried := StrIntTupleSet.add (last, !pi) !tried;
                                          ts := List.tl !ts;
                                  end
                        end
                else
                        begin
                          p := !p ^ towels.(!ti);
                          ts := [towels.(!ti)] @ !ts;
                          pi := !pi + 1;
                        end;
        done;
        if !pi < 0 then false else true

let mem = Hashtbl.create 666
let rec all_possible pattern towels =
        if Hashtbl.mem mem pattern then
                Hashtbl.find mem pattern
        else
                if String.length pattern = 0 then
                        1
                else
                        begin
                                let sum = ref 0 in
                                Array.iter (fun t ->
                                        if String.starts_with ~prefix:t pattern then
                                                let lp = String.length pattern in
                                                let lt = String.length t in
                                                sum := !sum + (all_possible (String.sub pattern lt (lp - lt)) towels)
                                ) towels;
                                Hashtbl.add mem pattern !sum;
                                !sum
                        end


let solve1 =
        let towels, patterns = prepare_input input in
        List.filter (fun p -> is_possible p towels) patterns

let solve2 patterns =
        let towels, _ = prepare_input input in
        List.fold_left (fun acc p ->
                acc + (all_possible p towels)
        ) 0 patterns

let () = Printf.printf "part1: %d | part2: %d\n%!" (List.length solve1) (solve2 solve1);

