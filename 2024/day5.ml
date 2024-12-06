open Containers

module PageOrdering = struct
  module IntDict = Hashtbl.Make' (Int)

  type t = int list IntDict.t

  let make rules =
    let ordering = IntDict.create 1000 in
    List.iter (fun (p1, p2) -> IntDict.add_list ordering p1 p2) rules;
    ordering

  let compare ordering p1 p2 =
    let compare' p1 p2 =
      IntDict.get ordering p1
      |> Option.map (fun successors -> if List.mem p2 successors then -1 else 1)
    in
    (* we store the less-than relationship in `ordering`. For example, given 12|23,
       if it is saying 12 should come before 23, and we store it like dict[12 => [23]].
       `compare'` works for `compare' 12 23`, but not `compare' 23 12`.
       To support the latter, additionally we do `compare' 23 12 |> neg`
    *)
    let open Option.Infix in
    compare' p1 p2
    <+> (compare' p2 p1 |> Option.map Int.neg)
    |> Option.get_or ~default:0

  let parser =
    let open Day3.Parser in
    let rules_parser =
      repeat (line (pair (number 2 <* const '|') (number 2)))
    in
    let updates_parser = repeat (line (separated ',' (number 2))) in
    pair (rules_parser <* const '\n') updates_parser

  let parse input = Day3.Parser.run input parser |> Result.get_exn
end

module Update = struct
  type t = int array

  let sort ordering = Array.sorted (PageOrdering.compare ordering)

  let make_opt update ordering =
    let update = Array.of_list update in
    let expected = sort ordering update in
    if Array.equal Int.equal expected update then Some update else None

  let make_correct update ordering =
    let update = Array.of_list update in
    let expected = sort ordering update in
    let corrected = Array.equal Int.equal expected update |> not in
    (expected, corrected)

  let get_middle update =
    let n = Array.length update in
    update.(n / 2)
end

let part1 input =
  let rules, updates = PageOrdering.parse input in
  let ordering = PageOrdering.make rules in
  updates
  |> List.filter_map (fun update ->
         let open Update in
         make_opt update ordering |> Option.map get_middle)
  |> Util.sum

let%expect_test "part1" =
  let test =
    {|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|}
  in
  part1 test |> Printf.printf "%d\n";
  [%expect {|
  143 |}];

  Util.with_data_file "day5.txt" (fun ic ->
      IO.read_all ic |> part1 |> string_of_int |> print_endline);
  [%expect {| 4924 |}]

let part2 input =
  let rules, updates = PageOrdering.parse input in
  let ordering = PageOrdering.make rules in
  updates
  |> List.filter_map (fun update ->
         let open Update in
         let update', corrected = make_correct update ordering in
         if corrected then Some (get_middle update') else None)
  |> Util.sum

let%expect_test "part2" =
  let test =
    {|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|}
  in
  part2 test |> Printf.printf "%d\n";
  [%expect {|
  123 |}];

  Util.with_data_file "day5.txt" (fun ic ->
      IO.read_all ic |> part2 |> string_of_int |> print_endline);
  [%expect {| 6085 |}]
