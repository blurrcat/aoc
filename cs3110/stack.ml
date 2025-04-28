open Containers

type 'a t = 'a list [@@deriving show]

let empty = []
let push item stack = item :: stack
let peak stack = Containers.List.head_opt stack

let pop stack =
  match stack with [] -> (None, stack) | head :: rest -> (Some head, rest)

let%expect_test "stack" =
  let inspect s = show Int.pp s |> print_endline in
  let stack = empty in
  let stack2 = push 1 stack in
  inspect stack2;
  [%expect {| [1] |}];
  let stack3 = push 2 stack2 in
  inspect stack3;
  [%expect {| [2; 1] |}];
  let head = peak stack3 in
  Format.printf "%a" (Option.pp Int.pp) head;
  [%expect {| Some 2 |}];
  let popped, stack4 = pop stack3 in
  inspect stack4;
  [%expect {| [1] |}];
  Format.printf "%a" (Option.pp Int.pp) popped;
  [%expect {| Some 2 |}]
