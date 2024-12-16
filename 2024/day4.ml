open Containers

module Graph : sig
  module Index : sig
    type t = int * int

    val move : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t Hash.t
    val pp : Format.formatter -> t -> unit
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
  end

  type 'c t

  val directions : Index.t list
  val of_strings : string list -> char t
  val foldi : ('a -> Index.t -> 'c -> 'a) -> 'a -> 'c t -> 'a
  val iteri : (Index.t -> 'c -> unit) -> 'c t -> unit
  val find_opt : ('c -> bool) -> 'c t -> (Index.t * 'c) option
  val get : 'c t -> Index.t -> 'c
  val set : 'c t -> Index.t -> 'c -> unit
  val get_opt : 'c t -> Index.t -> 'c option
  val get_path : 'c t -> Index.t -> Index.t -> int -> 'c list
  val is_valid_index : 'c t -> Index.t -> bool
  val pp : 'c Format.printer -> Format.formatter -> 'c t -> unit
end = struct
  module Index = struct
    type t = int * int

    let ( + ) = Pair.map_same2 Int.add
    let ( - ) = Pair.map_same2 Int.sub
    let move = ( + )
    let equal = Pair.equal Int.equal Int.equal
    let compare = Pair.compare Int.compare Int.compare
    let hash = Hash.pair Hash.int Hash.int
    let pp = Pair.pp Int.pp Int.pp
  end

  type 'c t = { data : 'c array array; size : int * int }

  let of_strings ss =
    let sx = List.length ss in
    let sy = List.hd ss |> String.length in
    let data = ss |> Array.of_list |> Array.map String.to_array in
    { data; size = (sx, sy) }

  let directions =
    [ (-1, -1); (-1, 0); (-1, 1); (0, 1); (0, -1); (1, -1); (1, 0); (1, 1) ]

  let is_valid_index g (i, j) =
    let sx, sy = g.size in
    i >= 0 && i < sx && j >= 0 && j < sy

  let get g (i, j) = g.data.(i).(j)
  let get_opt g idx = if is_valid_index g idx then Some (get g idx) else None
  let set g (i, j) c = g.data.(i).(j) <- c

  let iteri f g =
    let sx, sy = g.size in
    for i = 0 to sx - 1 do
      for j = 0 to sy - 1 do
        f (i, j) (get g (i, j))
      done
    done

  let foldi f initial g =
    let acc = ref initial in
    iteri (fun idx c -> acc := f !acc idx c) g;
    !acc

  let find_opt test =
    foldi
      (fun r idx c ->
        if Option.is_some r then r else if test c then Some (idx, c) else None)
      None

  let get_path g idx (di, dj) n =
    List.range 0 (n - 1)
    |> List.map (fun s -> Index.move idx (di * s, dj * s))
    |> List.filter (is_valid_index g)
    |> List.map (get g)

  let pp pc fmt g =
    let open Format in
    let row_pp = Format.array ~sep:(return " ") pc in
    Format.array ~sep:(return "@.") row_pp fmt g.data
end

let part1 lines =
  let g = Graph.of_strings lines in
  Graph.foldi
    (fun acc idx c ->
      match c with
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
  Graph.foldi
    (fun acc idx c ->
      match c with
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
