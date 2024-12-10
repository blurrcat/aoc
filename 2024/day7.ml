open Containers

module Equation = struct
  type t = int * int list
  type op_t = int -> int -> int

  let is_valid : op_t list -> t -> bool =
   fun operators (expected, numbers) ->
    let n = List.length numbers in
    let operator_combos =
      List.replicate (n - 1) (List.to_iter operators) |> Iter.seq_list
    in
    let is_expected operators =
      match numbers with
      | initial :: rest ->
          if
            List.fold_left2
              (fun acc op num -> op acc num)
              initial operators rest
            = expected
          then Some ()
          else None
      | [] -> if expected = 0 then Some () else None
    in
    Iter.find is_expected operator_combos |> Option.is_some

  let make = Pair.make

  let parser =
    let open Parse in
    let open CCParse.Infix in
    pure make <*> U.int <* string ":" <*> sep ~by:(string " ") U.int

  let parse = Parse.parse_string_exn parser
end

let solution operators lines =
  Iter.map Equation.parse lines
  |> Iter.filter (Equation.is_valid operators)
  |> Iter.map (fun (expected, _) -> expected)
  |> Iter.sum

let part1 = solution [ Int.add; Int.mul ]

let test =
  {|190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
|}

let%expect_test "part1" =
  String.lines test |> List.to_iter |> part1 |> string_of_int |> print_endline;
  [%expect {| 3749 |}];

  Util.with_data_file "day7.txt" (fun ic ->
      IO.read_lines_iter ic |> part1 |> Util.print_int);
  [%expect {| 2501605301465 |}]

let part2 =
  let concat n1 n2 = string_of_int n1 ^ string_of_int n2 |> int_of_string in
  solution [ Int.add; Int.mul; concat ]

let%expect_test "part2" =
  String.lines test |> List.to_iter |> part2 |> string_of_int |> print_endline;
  [%expect {| 11387 |}];

  Util.with_data_file "day7.txt" (fun ic ->
      IO.read_lines_iter ic |> part2 |> Util.print_int);
  [%expect {| 44841372855953 |}]
