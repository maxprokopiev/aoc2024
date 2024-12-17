let insts = ref [||]
let ip = ref 0
let a = ref 0
let b = ref 0
let c = ref 0
let res = ref ""
let part2 = ref false

let pow a b =
        let rec pow' a b acc =
                if b = 0 then acc
                else pow' a (b-1) (acc*a)
        in
        pow' a b 1

let combos = [|Fun.const 0;Fun.const 1;Fun.const 2;Fun.const 3;(fun () -> !a);(fun () -> !b);(fun () -> !c);(fun () -> -1)|]

let adv operand =
        let n = !a in
        let d = pow 2 (combos.(operand)()) in
        a := n / d

let bxl operand =
        b := !b lxor operand

let bst operand =
        b := combos.(operand)() mod 8

let jnz operand =
        if !a <> 0 then
                ip := operand - 2

let bxc _ =
        b := !b lxor !c

let ra = ref 0
let out operand =
        let _ = if String.length !res > 0 then let _ = res := !res ^ "," in () in
        res := !res ^ (string_of_int ((combos.(operand)()) mod 8))

let bdv operand =
        let n = !a in
        let d = pow 2 (combos.(operand)()) in
        b := n / d

let cdv operand =
        let n = !a in
        let d = pow 2 (combos.(operand)()) in
        c := n / d

let ops = [|adv;bxl;bst;jnz;bxc;out;bdv;cdv|]

let run () =
        ip := 0; b := 0; c := 0; res := "";
        while !ip < Array.length !insts do
                let op = ops.(!insts.(!ip)) in
                let operand = !insts.(!ip+1) in
                let _ = op operand in
                let _ = ip := !ip + 2 in ()
        done

let solve1 =
        a := int_of_string Sys.argv.(1);
        insts := Array.of_list(List.map int_of_string (String.split_on_char ',' Sys.argv.(2)));
        run (); !res

let solve2 =
        part2 := true;
        res := "";
        let _ = ra := 1 in
        let ast = ref 0 in
        while !res <> Sys.argv.(2) do
                ast := !ast + 1;
                ra := (!ast * (pow 8 8)) + (int_of_string "0o511352304632");
                a := !ra;
                run ()
        done;
        !ra

let () = Printf.printf "part1: %s | part2: %d\n%!" solve1 solve2;

