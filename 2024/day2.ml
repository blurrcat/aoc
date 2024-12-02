open Containers

let parse =
  List.map (fun l -> String.split_on_char ' ' l |> List.map int_of_string)

let differential l =
  let shifted = List.drop 1 l in
  List.combine_shortest l shifted |> List.map (fun (a, b) -> b - a)

let is_safe1 report =
  let delta = differential report in
  let open Int in
  List.for_all (fun d -> d >= 1 && d <= 3) delta
  || List.for_all (fun d -> d >= -3 && d <= -1) delta

let solution is_safe lines =
  lines |> parse |> List.count is_safe |> string_of_int

let part1 = solution is_safe1

let%expect_test "part1" =
  let test = {|7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9|} in
  test |> String.lines |> part1 |> print_endline;
  [%expect {| 2 |}];

  Util.with_data_file "day2.txt" (fun ic ->
      IO.read_lines_l ic |> part1 |> print_endline);
  [%expect {| 224 |}]

let is_safe2 report =
  (* generate candidate and check if it's safe *)
  let rec loop before after =
    match after with
    (* didn't find solution and we've reached the end *)
    | [] -> false
    (* candidate available *)
    | current :: rest ->
        let open CCList.Infix in
        let candiate = before @ rest in
        if is_safe1 candiate then true else loop (before @ [ current ]) rest
  in
  if is_safe1 report then true else loop [] report

let part2 = solution is_safe2

let%expect_test "part2" =
  let test = {|7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9|} in
  test |> String.lines |> part2 |> print_endline;
  [%expect {| 4 |}];

  Util.with_data_file "day2.txt" (fun ic ->
      IO.read_lines_l ic |> part2 |> print_endline);
  [%expect {|
       293 |}]
