open Containers
module Graph = Day4.Graph
module Index = Graph.Index

let directions = [ (-1, 0); (1, 0); (0, -1); (0, 1) ]

let make_map input =
  input |> String.lines
  |> List.map (fun row ->
         row |> String.trim |> String.to_list
         |> List.map (fun c -> Char.to_string c |> int_of_string))
  |> Graph.of_lists

let find_all_trailends start map =
  let rec loop idx =
    let c = Graph.get map idx in
    if Int.equal c 9 then [ idx ]
    else
      directions
      |> List.map (fun d ->
             let next = Index.(idx + d) in
             match Graph.get_opt map next with
             | Some next_c -> if Int.equal next_c (c + 1) then loop next else []
             | None -> [])
      |> List.flatten
  in
  loop start

let find_trailends_unique start map =
  find_all_trailends start map |> List.uniq ~eq:Index.equal

let solution find_trailends input =
  let map = make_map input in
  let trailheads = Graph.find_all (Int.equal 0) map in
  trailheads |> List.map (fun (trailhead, _c) -> find_trailends trailhead map)

let part1 = solution find_all_trailends
let part2 = solution find_trailends_unique

let test part input =
  let trailends = part input in
  let score = List.map List.length trailends |> Util.sum in
  Format.(
    printf "@[%a@]@.score: %d"
      (List.pp ~pp_sep:(return ";@.") (list Index.pp))
      trailends score)

let%expect_test "part1" =
  test part1 {|0123
         1234
         8765
         9876|};
  [%expect
    {|
    (3,0), (3,0), (3,0), (3,0), (3,0), (3,0), (3,0), (3,0), (3,0), (3,0), (3,0),
    (3,0), (3,0), (3,0), (3,0), (3,0)
    score: 16 |}];
  test part1
    {|89010123
      78121874
      87430965
      96549874
      45678903
      32019012
      01329801
      10456732|};
  [%expect
    {|
  (0,1), (3,0), (3,4), (5,4), (4,5);
  (2,5), (2,5), (4,5), (3,4), (2,5), (2,5), (4,5),
  (3,4);
  (0,1), (3,0), (3,4), (5,4),
  (4,5);
  (2,5), (2,5), (4,5),
  (3,4);
  (6,4);
  (2,5), (2,5), (4,5),
  (3,4);
  (3,4), (5,4), (4,5), (0,1), (3,0), (3,4), (5,4), (4,5), (0,1),
  (3,0);
  (3,4), (5,4), (4,5), (0,1), (3,0), (3,4), (5,4), (4,5), (0,1), (3,0), (3,4),
  (5,4), (4,5), (0,1), (3,0), (3,4), (5,4), (4,5), (0,1), (3,0), (2,5), (2,5),
  (4,5),
  (3,4);
  (3,4), (5,4), (4,5), (0,1), (3,0), (3,4), (5,4), (4,5), (0,1), (3,0), (3,4),
  (5,4), (4,5), (0,1), (3,0), (3,4), (5,4), (4,5), (0,1),
  (3,0)
  score: 81 |}];

  Util.test_with_data_file "day10.txt" (fun input ->
      part1 input |> List.map List.length |> Util.sum);
  [%expect {| result is 1094 |}]

let%expect_test "part2" =
  test part2
    {|89010123
      78121874
      87430965
      96549874
      45678903
      32019012
      01329801
      10456732|};
  [%expect
    {|
  (0,1), (3,0), (3,4), (5,4), (4,5);
  (2,5), (4,5),
  (3,4);
  (0,1), (3,0), (3,4), (5,4),
  (4,5);
  (2,5), (4,5),
  (3,4);
  (6,4);
  (2,5), (4,5),
  (3,4);
  (3,4), (5,4), (4,5), (0,1),
  (3,0);
  (5,4), (0,1), (3,0), (2,5), (4,5),
  (3,4);
  (3,4), (5,4), (4,5), (0,1),
  (3,0)
  score: 36 |}];

  Util.test_with_data_file "day10.txt" (fun input ->
      part2 input |> List.map List.length |> Util.sum);
  [%expect {| result is 482 |}]
