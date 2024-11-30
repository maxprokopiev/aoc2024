let input =  Utils.Files.read_file "day01/sample.txt"
let lines = String.split_on_char '\n'
let () = Printf.printf "%s\n%!" (List.hd (lines input))
