open Containers

module Graph : sig
  module Index : sig
    type t = int * int

    val move : t -> t -> t
  end

  type t

  val directions : Index.t list
  val of_strings : string list -> t
  val fold : ('a -> Index.t -> 'a) -> 'a -> t -> 'a
  val get : t -> Index.t -> char
  val get_path : t -> Index.t -> Index.t -> int -> char list
end = struct
  module Index = struct
    type t = int * int

    let move index delta = Pair.map_same2 Int.add index delta
  end

  type t = { data : char array array; size : int * int }

  let of_strings ss =
    let sx = List.length ss in
    let sy = List.hd ss |> String.length in
    let data = ss |> Array.of_list |> Array.map String.to_array in
    { data; size = (sx, sy) }

  let directions =
    [ (-1, -1); (-1, 0); (-1, 1); (0, 1); (0, -1); (1, -1); (1, 0); (1, 1) ]

  let iteri : (Index.t -> unit) -> t -> unit =
   fun f { size = sx, sy; _ } ->
    for i = 0 to sx - 1 do
      for j = 0 to sy - 1 do
        f (i, j)
      done
    done

  let fold f initial g =
    let acc = ref initial in
    iteri (fun idx -> acc := f !acc idx) g;
    !acc

  let is_valid_index g (i, j) =
    let sx, sy = g.size in
    i >= 0 && i < sx && j >= 0 && j < sy

  let get g (i, j) = g.data.(i).(j)

  let get_path g idx (di, dj) n =
    List.range 0 (n - 1)
    |> List.map (fun s -> Index.move idx (di * s, dj * s))
    |> List.filter (is_valid_index g)
    |> List.map (get g)
end

let part1 lines =
  let g = Graph.of_strings lines in
  Graph.fold
    (fun acc idx ->
      match Graph.get g idx with
      | 'X' ->
          let matches =
            Graph.directions
            |> List.map (fun direction ->
                   let path =
                     Graph.get_path g idx direction 4 |> String.of_list
                   in
                   if String.equal path "XMAS" then 1 else 0)
            |> Util.sum
          in
          acc + matches
      | _ -> acc)
    0 g

let%expect_test "part1" =
  let test =
    {|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|}
    |> String.lines
  in
  part1 test |> string_of_int |> print_endline;
  [%expect {|
  18 |}];

  Util.with_data_file "day4.txt" (fun ic ->
      IO.read_lines_l ic |> part1 |> string_of_int |> print_endline);
  [%expect {|
    2427 |}]

let part2 lines =
  let g = Graph.of_strings lines in
  Graph.fold
    (fun acc idx ->
      match Graph.get g idx with
      | 'A' ->
          let count_mas =
            (* delta to the corners relative to 'A' *)
            [ (-1, -1); (1, 1); (1, -1); (-1, 1) ]
            |> List.map (fun direction ->
                   let corner = Graph.Index.move idx direction in
                   (* for each corner, find the path from the corner to 'A' *)
                   let opposite_direction = Pair.map_same Int.neg direction in
                   let path =
                     Graph.get_path g corner opposite_direction 3
                     |> String.of_list
                   in
                   (* count number of "MAS" matches *)
                   if String.equal path "MAS" then 1 else 0)
            |> Util.sum
          in
          (* if there are exactly 2 "MAS" matches, then we found an X-MAS *)
          if Int.equal count_mas 2 then acc + 1 else acc
      | _ -> acc)
    0 g

let%expect_test "part2" =
  let test =
    {|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|}
    |> String.lines
  in
  part2 test |> string_of_int |> print_endline;
  [%expect {|
  9 |}];

  Util.with_data_file "day4.txt" (fun ic ->
      IO.read_lines_l ic |> part2 |> string_of_int |> print_endline);
  [%expect {|
    1900 |}]
