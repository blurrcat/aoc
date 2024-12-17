open Containers

module NumCount = struct
  type t = int * int

  let hash = Hash.(pair int int)
  let equal = Equal.(pair int int)

  let num_digits num =
    let rec count digits n =
      if n = 0 then digits else count (digits + 1) (n / 10)
    in
    count 0 num

  let split_number left_digits num =
    let rec power10 acc n =
      match n with 0 -> acc | _ -> power10 (acc * 10) (n - 1)
    in
    let divisor = power10 1 left_digits in
    let left = num / divisor in
    let right = num - (left * divisor) in
    [ left; right ]

  let blink_one (num, count) =
    if num = 0 then [ (1, count) ]
    else
      let n_digits = num_digits num in
      if n_digits mod 2 = 0 then
        split_number (n_digits / 2) num |> List.map (fun n -> (n, count))
      else [ (num * 2024, count) ]

  let of_numbers = Iter.count ~hash:Int.hash ~eq:Int.equal

  module Dict = Hashtbl.Make' (Int)

  let group_num_count ncs =
    let dict = Dict.create 10000 in
    Iter.iter (fun (num, count) -> Dict.incr ~by:count dict num) ncs;
    Dict.to_iter dict

  let blink numbers = numbers |> Iter.flat_map_l blink_one |> group_num_count
  let total_count = Iter.fold (fun acc (_, count) -> acc + count) 0

  let blinks times numbers =
    List.range 1 times
    |> List.fold_left (fun numbers _ -> blink numbers) numbers
    |> total_count
end

let parse input =
  input |> String.trim |> String.split_on_char ' ' |> List.map int_of_string
  |> List.to_iter

let solution times input =
  input |> parse |> NumCount.of_numbers |> NumCount.blinks times

let part1 = solution 25
let part2 = solution 75
let stones = [ 125; 17 ] |> List.to_iter

let%expect_test "test" =
  Util.test_with_data_file "day11.txt" part1;
  [%expect {|
  result is 220999 |}];
  stones |> NumCount.of_numbers |> NumCount.blinks 25 |> Util.print_int;
  [%expect {|
  55312 |}];

  Util.test_with_data_file "day11.txt" part2;
  [%expect {|
    result is 261936432123724 |}]
