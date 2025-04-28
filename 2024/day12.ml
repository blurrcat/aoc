open Containers
module Graph = Day4.Graph
module Index = Graph.Index
module IndexSet = Set.Make (Index)

module Garden = struct
  module Region = struct
    type t = char * IndexSet.t

    let make plant plots = (plant, IndexSet.of_list plots)
    let directions = [ (0, 1); (0, -1); (-1, 0); (1, 0) ]
    let area (_, plots) = plots |> IndexSet.to_iter |> Iter.length

    let perimeter (_, plots) =
      IndexSet.fold
        (fun plot acc ->
          List.fold_left
            (fun acc' direction ->
              let neighbor = Index.(plot + direction) in
              if IndexSet.mem neighbor plots then acc' else acc' + 1)
            acc directions)
        plots 0

    let price r = area r * perimeter r
    let pp : t Format.printer = Format.Dump.(pair char (IndexSet.pp Index.pp))
  end

  module Side = struct
    type direction_t = Vertical | Horizontal
    type t = direction_t * Index.t

    let pp_direction fmt = function
      | Vertical -> Format.string fmt "V"
      | Horizontal -> Format.string fmt "H"

    let pp = Format.(pair ~sep:(return "-") pp_direction Index.pp)

    let neighbors (d, idx) =
      let directions =
        match d with
        | Vertical -> [ (0, 1); (0, -1) ]
        | Horizontal -> [ (1, 0); (-1, 0) ]
      in
      directions |> List.map (Index.move idx)

    let edges (_, plots) =
      IndexSet.fold
        (fun plot sides ->
          List.fold_left
            (fun acc' direction ->
              let neighbor = Index.(plot + direction) in
              if IndexSet.mem neighbor plots then acc'
              else
                let side =
                  match direction with
                  | _, 0 -> (Vertical, neighbor)
                  | 0, _ -> (Horizontal, neighbor)
                  | _ -> failwith "unexpected direction"
                in
                side :: acc')
            sides Region.directions)
        plots []

    let sides region =
      let edges' = edges region in
      let edges_set = edges' |> List.map Util.snd |> IndexSet.of_list in
      let n = List.length edges' in
      let connected =
        List.fold_left
          (fun acc edge ->
            let acc' =
              List.fold_left
                (fun acc' neighbor ->
                  if IndexSet.mem neighbor edges_set then acc' + 1 else acc')
                acc (neighbors edge)
            in
            acc')
          0 edges'
      in
      let result = n - (connected / 2) in
      result

    let price region = Region.area region * sides region
  end

  type t = Region.t list

  let pp = Format.(list ~sep:(return "@.") Region.pp)

  let price regions =
    regions |> List.to_iter |> Iter.map Region.price |> Iter.sum

  let discounted_price regions =
    regions |> List.to_iter |> Iter.map Side.price |> Iter.sum

  let parse : string -> t =
   fun s ->
    let map =
      s |> String.trim |> String.lines |> List.map String.trim
      |> Graph.of_strings
    in
    let rows, cols = Graph.size map in
    let visited = Array.make_matrix rows cols false in
    let is_visited (i, j) = visited.(i).(j) in
    let set_visited (i, j) = visited.(i).(j) <- true in

    let rec walk region expected_plant idx =
      if Char.equal (Graph.get map idx) expected_plant then (
        set_visited idx;
        let region' = idx :: region in
        List.fold_left
          (fun acc direction ->
            let neighbor = Index.(idx + direction) in
            match Graph.get_opt map neighbor with
            | Some _ ->
                if is_visited neighbor then acc
                else walk acc expected_plant neighbor
            | None -> acc)
          region' Region.directions)
      else region
    in

    Graph.foldi
      (fun regions idx plant ->
        if is_visited idx then regions
        else
          let region = walk [] plant idx in
          Region.make plant region :: regions)
      [] map
end

let part1 input = Garden.parse input |> Garden.price

let test price input =
  let garden = Garden.parse input in
  let price = price garden in
  Format.(printf "@[%a@]@.price: %d@." Garden.pp garden price)

let%expect_test "part1" =
  let input1 = {|
    AAAA
    BBCD
    BBCC
    EEEC
  |} in
  let input2 =
    {|
    RRRRIICCFF
    RRRRIICCCF
    VVRRRCCFFF
    VVRCCCJFFF
    VVVVCJJCFE
    VVIVCCJJEE
    VVIIICJJEE
    MIIIIIJJEE
    MIIISIJEEE
    MMMISSJEEE|}
  in
  test Garden.price input1;
  [%expect
    {|
    (E, (3,0), (3,1), (3,2))
    (D, (1,3))
    (C, (1,2), (2,2), (2,3), (3,3))
    (B, (1,0), (1,1), (2,0), (2,1))
    (A, (0,0), (0,1), (0,2), (0,3))
    price: 140 |}];
  test Garden.price input2;
  [%expect
    {|
  (S, (8,4), (9,4), (9,5))
  (M, (7,0), (8,0), (9,0), (9,1), (9,2))
  (I, (5,2), (6,2), (6,3), (6,4), (7,1), (7,2), (7,3), (7,4), (7,5), (8,1),
   (8,2), (8,3), (8,5), (9,3))
  (E, (4,9), (5,8), (5,9), (6,8), (6,9), (7,8), (7,9), (8,7), (8,8), (8,9),
   (9,7), (9,8), (9,9))
  (C, (4,7))
  (J, (3,6), (4,5), (4,6), (5,6), (5,7), (6,6), (6,7), (7,6), (7,7), (8,6),
   (9,6))
  (V, (2,0), (2,1), (3,0), (3,1), (4,0), (4,1), (4,2), (4,3), (5,0), (5,1),
   (5,3), (6,0), (6,1))
  (F, (0,8), (0,9), (1,9), (2,7), (2,8), (2,9), (3,7), (3,8), (3,9), (4,8))
  (C, (0,6), (0,7), (1,6), (1,7), (1,8), (2,5), (2,6), (3,3), (3,4), (3,5),
   (4,4), (5,4), (5,5), (6,5))
  (I, (0,4), (0,5), (1,4), (1,5))
  (R, (0,0), (0,1), (0,2), (0,3), (1,0), (1,1), (1,2), (1,3), (2,2), (2,3),
   (2,4), (3,2))
  price: 1930 |}];

  Util.test_with_data_file "day12.txt" part1;
  [%expect {| result is 1573474 |}]

let part2 input = input |> Garden.parse |> Garden.discounted_price

let%expect_test "part2" =
  let input1 = {|
    AAAA
    BBCD
    BBCC
    EEEC
    |} in
  test Garden.discounted_price input1;
  [%expect
    {|
  (E, (3,0), (3,1), (3,2))
  (D, (1,3))
  (C, (1,2), (2,2), (2,3), (3,3))
  (B, (1,0), (1,1), (2,0), (2,1))
  (A, (0,0), (0,1), (0,2), (0,3))
  price: 80 |}];
  let input2 = {|
    EEEEE
    EXXXX
    EEEEE
    EXXXX
    EEEEE
  |} in
  test Garden.discounted_price input2;
  [%expect
    {|
  (X, (3,1), (3,2), (3,3), (3,4))
  (X, (1,1), (1,2), (1,3), (1,4))
  (E, (0,0), (0,1), (0,2), (0,3), (0,4), (1,0), (2,0), (2,1), (2,2), (2,3),
   (2,4), (3,0), (4,0), (4,1), (4,2), (4,3), (4,4))
  price: 236 |}];

  let input3 =
    {|
    AAAAAA
    AAABBA
    AAABBA
    ABBAAA
    ABBAAA
    AAAAAA
  |}
  in
  test Garden.discounted_price input3;
  [%expect
    {|
  (B, (3,1), (3,2), (4,1), (4,2))
  (B, (1,3), (1,4), (2,3), (2,4))
  (A, (0,0), (0,1), (0,2), (0,3), (0,4), (0,5), (1,0), (1,1), (1,2), (1,5),
   (2,0), (2,1), (2,2), (2,5), (3,0), (3,3), (3,4), (3,5), (4,0), (4,3), (4,4),
   (4,5), (5,0), (5,1), (5,2), (5,3), (5,4), (5,5))
  price: 368 |}];

  Util.test_with_data_file "day12.txt" part2;
  [%expect {|
    result is 917390 |}]
