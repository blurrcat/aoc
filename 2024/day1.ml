open Containers

let parse : string list -> int list * int list =
  let open CCFun.Infix in
  List.map (fun l -> Scanf.sscanf l "%d %d" (fun x y -> (x, y))) %> List.split

let part1 : string list -> string =
 fun lines ->
  let col1, col2 = parse lines |> Pair.map_same (List.sort Int.compare) in
  List.map2 (fun n1 n2 -> abs (n1 - n2)) col1 col2 |> Util.sum |> string_of_int

let%expect_test "part1" =
  let test = {|3   4
4   3
2   5
1   3
3   9
3   3|} in
  test |> String.lines |> part1 |> print_endline;
  [%expect {| 11 |}];

  Util.with_data_file "day1.txt" (fun ic ->
      IO.read_lines_l ic |> part1 |> print_endline);
  [%expect {|
    2970687 |}]

let part2 : string list -> string =
 fun lines ->
  let col1, col2 = parse lines in
  let module IntHashtbl = Hashtbl.Make' (Int) in
  let counter = IntHashtbl.create 1000 in
  List.iter (IntHashtbl.incr counter) col2;
  col1
  |> List.map (fun n -> n * IntHashtbl.get_or counter n ~default:0)
  |> Util.sum |> string_of_int

let%expect_test "part2" =
  let test = {|3   4
4   3
2   5
1   3
3   9
3   3|} in
  test |> String.lines |> part2 |> print_endline;
  [%expect {| 31 |}];

  Util.with_data_file "day1.txt" (fun ic ->
      IO.read_lines_l ic |> part2 |> print_endline);
  [%expect {|
    23963899 |}]
