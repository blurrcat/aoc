open Containers
module Graph = Day4.Graph
module IndexSet = Set.Make (Graph.Index)

let turn_right dir =
  match dir with
  | '^' -> '>'
  | 'v' -> '<'
  | '<' -> '^'
  | '>' -> 'v'
  | _ -> failwith ("unexpected direction: " ^ Char.to_string dir)

let find_start map =
  Graph.find_opt (fun c -> List.mem ~eq:Char.equal c [ '^'; 'v'; '>'; '<' ]) map
  |> Option.get_exn_or "not found"

let get_next_pos pos dir =
  let delta =
    match dir with
    | '^' -> (-1, 0)
    | 'v' -> (1, 0)
    | '<' -> (0, -1)
    | '>' -> (0, 1)
    | _ -> (0, 0)
  in
  Graph.Index.move pos delta

let rec walk : char Graph.t -> IndexSet.t -> Graph.Index.t -> char -> IndexSet.t
    =
 fun map path pos dir ->
  let path = IndexSet.add pos path in
  let next_pos = get_next_pos pos dir in
  match Graph.get_opt map next_pos with
  | Some '.' | Some '^' -> walk map path next_pos dir
  | Some '#' -> walk map path pos (turn_right dir)
  | Some _ -> failwith "unexpected tile"
  | None -> path

let part1 input =
  let map = input |> String.lines |> Graph.of_strings in
  let start_pos, start_dir = find_start map in
  walk map IndexSet.empty start_pos start_dir |> IndexSet.to_iter |> Iter.length

module IndexDirSet = Set.Make (struct
  type t = Graph.Index.t * char

  let compare = Pair.compare Graph.Index.compare Char.compare
end)

let part2 input =
  let map = input |> String.lines |> Graph.of_strings in
  let start_pos, start_dir = find_start map in
  (* only need to test obstrcutions on the path *)
  let path = walk map IndexSet.empty start_pos start_dir in
  IndexSet.fold
    (fun new_obstruction total ->
      let get_opt p =
        if Graph.Index.equal p new_obstruction then Some '#'
        else Graph.get_opt map p
      in
      let rec is_loop path pos dir =
        if IndexDirSet.mem (pos, dir) path then true
        else
          let path' = IndexDirSet.add (pos, dir) path in
          let next_pos = get_next_pos pos dir in
          match get_opt next_pos with
          | Some '.' | Some '^' -> is_loop path' next_pos dir
          | Some '#' -> is_loop path' pos (turn_right dir)
          | Some tile -> failwith ("unexpected tile" ^ Char.to_string tile)
          | None -> false
      in
      if is_loop IndexDirSet.empty start_pos start_dir then total + 1 else total)
    path 0

let test =
  {|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...|}

let%expect_test "part1" =
  part1 test |> Printf.printf "%d";
  [%expect {|
  41 |}];

  Util.with_data_file "day6.txt" (fun ic ->
      IO.read_all ic |> part1 |> Printf.printf "%d");
  [%expect {| 4647 |}]

let%expect_test "part2" =
  part2 test |> Printf.printf "%d";
  [%expect {|
  6 |}];

  Util.with_data_file "day6.txt" (fun ic ->
      IO.read_all ic |> part2 |> Printf.printf "%d");
  [%expect {| 1723 |}]
