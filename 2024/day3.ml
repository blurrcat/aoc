open Containers

let show_result string_of_a r =
  let s =
    match r with
    | Ok a -> Printf.sprintf "Ok %s" (string_of_a a)
    | Error e -> Printf.sprintf "Error %s" e
  in
  print_endline s

module Parser = struct
  type 'a t = string -> ('a * string, string) result

  let run : string -> 'a t -> ('a, string) result =
   fun s p ->
    p s
    |> Result.flat_map (fun (c, rest) ->
           if String.is_empty rest then Ok c
           else Error (Printf.sprintf "expected EOF; got %s" rest))

  let pure : 'a -> 'a t = fun a s -> Ok (a, s)

  let map : ('a -> 'b) -> 'a t -> 'b t =
   fun f p s -> p s |> Result.map (fun (c, rest) -> (f c, rest))

  let ( <$> ) = map

  let take1_when : (char -> bool) -> string -> 'a t =
   fun test e s ->
    if String.is_empty s then Error "empty string"
    else
      let c = String.get s 0 in
      if test c then Ok (c, String.drop 1 s)
      else Error (Printf.sprintf "expected %s; got %c" e c)

  let char : char list -> char t =
   fun expected ->
    take1_when
      (fun c -> List.mem ~eq:Char.equal c expected)
      (Printf.sprintf "any char of %s" (String.of_list expected))

  let const : char -> char t =
   fun c -> take1_when (Char.equal c) (String.of_char c)

  let ignore1 : unit t =
    take1_when (fun _ -> true) "one char" |> map (Fun.const ())

  let ap : ('a -> 'b) t -> 'a t -> 'b t =
   fun parser_f parser s ->
    let open Result.Infix in
    let* f, rest = parser_f s in
    let* c, rest' = parser rest in
    Ok (f c, rest')

  let ( <*> ) = ap

  (* parse the given list of parsers which return the same type of result *)
  let many : 'a t list -> 'a list t =
   fun parsers s ->
    Result.fold_l
      (fun (acc, rest) parser ->
        parser rest |> Result.map (fun (r, rest') -> (acc @ [ r ], rest')))
      ([], s) parsers

  (* parse using the given parser repeatedly until EOF *)
  let repeat : 'a t -> 'a list t =
   fun parser s ->
    let rec loop acc rest =
      match parser rest with
      | Ok (r, "") -> Ok (acc @ [ r ], "")
      | Ok (r, rest') -> loop (acc @ [ r ]) rest'
      | Error e -> Error e
    in
    loop [] s

  let word : string -> string t =
   fun expected ->
    String.to_list expected |> List.map const |> many |> map String.of_list

  let digit : char t = "0123456789" |> String.to_list |> char
  let non_zero_digit : char t = "123456789" |> String.to_list |> char

  let number : int -> int t =
   fun n ->
    let digits =
      if n = 1 then [ digit ]
      else non_zero_digit :: List.replicate (n - 1) digit
    in
    digits |> many |> map (fun digits -> String.of_list digits |> int_of_string)

  let one_of : 'a t list -> 'a t =
   fun parsers s ->
    List.fold_while
      (fun _r parser ->
        match parser s with
        | Ok (rr, rest) -> (Ok (rr, rest), `Stop)
        | Error e -> (Error e, `Continue))
      (Error "none of the parsers match") parsers

  let%expect_test "parser" =
    (* const *)
    const 's' |> run "s" |> show_result String.of_char;
    [%expect {| Ok s |}];
    const 's' |> run "c" |> show_result String.of_char;
    [%expect {| Error expected s; got c |}];
    const 's' |> run "ss" |> show_result String.of_char;
    [%expect {| Error expected EOF; got s |}];

    (* digit *)
    digit |> run "1" |> show_result String.of_char;
    [%expect {| Ok 1 |}];
    digit |> run "s" |> show_result String.of_char;
    [%expect {| Error expected any char of 0123456789; got s |}];
    digit |> run "11" |> show_result String.of_char;
    [%expect {| Error expected EOF; got 1 |}];

    (* sequence *)
    word "MUL" |> run "MUL" |> show_result Fun.id;
    ();
    [%expect {| Ok MUL |}];

    one_of [ word "foo"; word "bar" ] |> run "foo" |> show_result Fun.id;
    [%expect {|
      Ok foo |}];
    one_of [ word "foo"; word "bar" ] |> run "bar" |> show_result Fun.id;
    [%expect {|
      Ok bar |}];
    one_of [ word "foo"; word "bar" ] |> run "baz" |> show_result Fun.id;
    [%expect {|
      Error expected r; got z |}];

    (* one_of *)
    let number2 = one_of [ number 2; number 1 ] in
    number2 |> run "0" |> show_result string_of_int;
    [%expect {| Ok 0 |}];
    number2 |> run "1" |> show_result string_of_int;
    [%expect {| Ok 1 |}];
    number2 |> run "10" |> show_result string_of_int;
    [%expect {| Ok 10 |}];
    number2 |> run "100" |> show_result string_of_int;
    ();
    [%expect {| Error expected EOF; got 0 |}];

    (* ap *)
    let ap1 = pure (fun a _b -> a) <*> number 1 <*> number 2 in
    ap1 |> run "123" |> show_result string_of_int;
    [%expect {| Ok 1 |}];
    let ap2 = pure (fun a _b -> a) <*> number 2 <*> number 1 in
    ap2 |> run "123" |> show_result string_of_int;
    [%expect {| Ok 12 |}]
end

let part1 input =
  let open Parser in
  let number = one_of [ number 3; number 2; number 1 ] in
  let multiply =
    pure (fun _mul n1 _comma n2 _closing_paren -> n1 * n2)
    <*> word "mul(" <*> number <*> word "," <*> number <*> word ")"
  in
  let maybe_multiply =
    one_of [ multiply |> map Option.some; ignore1 |> map (Fun.const None) ]
  in
  let parser =
    repeat maybe_multiply |> map (fun ns -> List.keep_some ns |> Util.sum)
  in
  run input parser

let%expect_test "part1" =
  let test =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  in
  part1 test |> show_result string_of_int;
  [%expect {|
    Ok 161 |}];

  Util.with_data_file "day3.txt" (fun ic ->
      IO.read_all ic |> part1 |> show_result string_of_int);
  [%expect {| Ok 180233229 |}]

module Term = struct
  type t = Mul of int * int | Do | Dont | Null

  let parser =
    let open Parser in
    let multiply =
      let number = one_of [ number 3; number 2; number 1 ] in
      pure (fun _mul n1 _comma n2 _closing_paren -> Mul (n1, n2))
      <*> word "mul(" <*> number <*> word "," <*> number <*> word ")"
    in
    let do' = word "do()" |> map (Fun.const Do) in
    let dont = word "don't()" |> map (Fun.const Dont) in
    let maybe_term =
      one_of [ multiply; dont; do'; ignore1 |> map (Fun.const Null) ]
    in
    repeat maybe_term
end

let part2 input =
  let open Term in
  Parser.run input parser
  |> Result.map (fun terms ->
         let total, _ =
           List.fold_left
             (fun (total, do_multiply) -> function
               | Mul (n1, n2) ->
                   if do_multiply then (total + (n1 * n2), do_multiply)
                   else (total, do_multiply)
               | Do -> (total, true)
               | Dont -> (total, false)
               | Null -> (total, do_multiply))
             (0, true) terms
         in
         total)

let%expect_test "part2" =
  let test =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  in
  part2 test |> show_result string_of_int;
  [%expect {|
    Ok 48 |}];

  Util.with_data_file "day3.txt" (fun ic ->
      IO.read_all ic |> part2 |> show_result string_of_int);
  [%expect {| Ok 95411583 |}]
