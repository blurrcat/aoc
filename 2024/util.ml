open Containers

let get_root () = Sys.getenv "DEVENV_ROOT"

let with_data_file path f =
  let path = get_root () ^ "/data/" ^ path in
  IO.with_in path f

let sum = List.fold_left Int.( + ) 0

let show_int_list ls =
  Printf.sprintf "[%s]" (List.map string_of_int ls |> String.concat "; ")
