let unique l =
  let rec aux l acc =
    match l with
    | [] ->
        List.rev acc
    | h :: t ->
        if List.mem h acc then aux t acc else aux t (h :: acc)
  in
  aux l []
