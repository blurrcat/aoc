open Containers

let get_root () = Sys.getenv "DEVENV_ROOT"

let with_in path f =
  let path = get_root () ^ "/data/" ^ path in
  IO.with_in path f

let sum = List.fold_left Int.( + ) 0
