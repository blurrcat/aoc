open Containers

let get_root () = Sys.getenv "DEVENV_ROOT"

let with_data_file path f =
  let path = get_root () ^ "/data/" ^ path in
  IO.with_in path f

let sum = List.fold_left Int.( + ) 0
let print_int = Printf.printf "%d\n"

let print_int_list tag ls =
  Printf.printf "%s: [%s]\n" tag
    (List.map string_of_int ls |> String.concat "; ")

let print_int_array tag ls =
  Printf.printf "%s: [%s]\n" tag
    (Array.map string_of_int ls |> Array.to_list |> String.concat "; ")
