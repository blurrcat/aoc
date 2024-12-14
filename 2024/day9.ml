open Containers

module File = struct
  type t = { address : int; id : int; size : int }

  let make ~address ~id ~size = { address; id; size }
  let free ~address ~size = { address; size; id = -1 }

  let checksum { address; id; size } =
    (Iter.int_range ~start:address ~stop:(address + size - 1) |> Iter.sum) * id

  let compare f1 f2 = Int.compare f1.address f2.address
  let is_free_space f = f.id = -1

  let pp fmt f =
    Format.(
      fprintf fmt "{ address: %d; id: %d; size: %d}" f.address f.id f.size)
end

module Disk = struct
  type t = { files : File.t list; free_spaces : File.t list; capacity : int }

  let make ~files ~free_spaces ~capacity =
    (* let last_address (fs : File.t list) = *)
    (*   match List.last_opt fs with Some f -> f.address + f.size | None -> 0 *)
    (* in *)
    let files = List.sort File.compare files in
    let free_spaces = List.sort File.compare free_spaces in
    (* let capacity = max (last_address files) (last_address free_spaces) in *)
    { files; free_spaces; capacity }

  let of_disk_map m =
    let files, free_spaces, address =
      m |> String.to_iter
      |> Iter.foldi
           (fun (files, free_spaces, address) i c ->
             let size = Char.to_string c |> int_of_string in
             let address' = address + size in
             if i mod 2 = 1 then
               (files, File.free ~address ~size :: free_spaces, address')
             else
               ( File.make ~address ~id:(i / 2) ~size :: files,
                 free_spaces,
                 address' ))
           ([], [], 0)
    in
    {
      files = List.rev files;
      free_spaces = List.rev free_spaces;
      capacity = address;
    }

  let to_disk_map d =
    let prefix =
      d.files @ d.free_spaces |> List.sort File.compare
      |> List.map (fun f ->
             if File.is_free_space f then String.repeat "." f.size
             else String.repeat (Int.to_string f.id) f.size)
      |> String.concat ""
    in
    let free_spaces = d.capacity - String.length prefix |> String.repeat "." in
    prefix ^ free_spaces

  let compact d =
    let rec loop : File.t list -> File.t list -> File.t list -> File.t list =
     fun files free_spaces new_files ->
      match (files, free_spaces) with
      | [], _ -> new_files
      | files', [] -> files' @ new_files
      | file :: other_files, free_space :: other_free_spaces ->
          if file.address < free_space.address then files @ new_files
          else if file.size <= free_space.size then
            (* copy file to free space *)
            let new_file = { file with address = free_space.address } in
            (* update address and size of the free space *)
            let free_space_rest =
              {
                free_space with
                size = free_space.size - file.size;
                address = free_space.address + file.size;
              }
            in
            loop other_files
              (free_space_rest :: other_free_spaces)
              (new_file :: new_files)
          else
            (* copy part of file to fill the free space *)
            let new_file = { free_space with id = file.id } in
            (* for the rest of the file, update its size *)
            let new_file_rest =
              { file with size = file.size - free_space.size }
            in
            loop
              (new_file_rest :: other_files)
              other_free_spaces (new_file :: new_files)
    in
    let files = loop (List.rev d.files) d.free_spaces [] in
    make ~files ~free_spaces:[] ~capacity:d.capacity

  let fold_map_while f ls =
    let rec loop acc = function
      | item :: rest -> (
          match f item with
          | `Continue -> loop (item :: acc) rest
          | `Stop (Some (item', result)) ->
              (List.rev (item' :: acc) @ rest, Some result)
          | `Stop None -> (List.rev (item :: acc) @ rest, None))
      | [] -> (List.rev acc, None)
    in
    loop [] ls

  let compact_whole_file d =
    let rec loop :
        File.t list -> File.t list -> File.t list -> File.t list * File.t list =
     fun files free_spaces new_files ->
      match files with
      | [] -> (new_files, free_spaces)
      | file :: other_files -> (
          let free_spaces', new_file_opt =
            fold_map_while
              (fun (free_space : File.t) ->
                if file.address < free_space.address then `Stop None
                else if free_space.size >= file.size then
                  let new_file = { file with address = free_space.address } in
                  let new_free_space =
                    {
                      free_space with
                      size = free_space.size - new_file.size;
                      address = free_space.address + file.size;
                    }
                  in
                  `Stop (Some (new_free_space, new_file))
                else `Continue)
              free_spaces
          in
          match new_file_opt with
          | Some new_file ->
              loop other_files free_spaces' (new_file :: new_files)
          | None -> loop other_files free_spaces (file :: new_files))
    in
    let files, free_spaces = loop (List.rev d.files) d.free_spaces [] in
    make ~files ~free_spaces ~capacity:d.capacity

  let checksum { files; _ } = List.map File.checksum files |> Util.sum

  let pp fmt d =
    Format.(
      fprintf fmt "{ files: %a; @. free_spaces: %a }" (list File.pp) d.files
        (list File.pp) d.free_spaces)
end

let test = "2333133121414131402"
(* let test = "12345" *)

let%expect_test "split" =
  let disk = Disk.of_disk_map test in

  Format.(printf "disk map: %s@.%a@." (Disk.to_disk_map disk) Disk.pp disk);
  [%expect
    {|
  disk map: 00...111...2...333.44.5555.6666.777.888899
  { files: { address: 0; id: 0; size: 2}, { address: 5; id: 1; size: 3},
  { address: 11; id: 2; size: 1}, { address: 15; id: 3; size: 3},
  { address: 19; id: 4; size: 2}, { address: 22; id: 5; size: 4},
  { address: 27; id: 6; size: 4}, { address: 32; id: 7; size: 3},
  { address: 36; id: 8; size: 4},
  { address: 40; id: 9; size: 2};
   free_spaces: { address: 2; id: -1; size: 3}, { address: 8; id: -1; size: 3},
  { address: 12; id: -1; size: 3}, { address: 18; id: -1; size: 1},
  { address: 21; id: -1; size: 1}, { address: 26; id: -1; size: 1},
  { address: 31; id: -1; size: 1}, { address: 35; id: -1; size: 1},
  { address: 40; id: -1; size: 0} } |}];

  let disk' = Disk.compact disk in
  Format.(
    printf "disk map: %a@.%s@.checksum: %d" Disk.pp disk'
      (Disk.to_disk_map disk') (Disk.checksum disk'));
  [%expect
    {|
  disk map: { files: { address: 0; id: 0; size: 2},
  { address: 2; id: 9; size: 2}, { address: 4; id: 8; size: 1},
  { address: 5; id: 1; size: 3}, { address: 8; id: 8; size: 3},
  { address: 11; id: 2; size: 1}, { address: 11; id: 7; size: 0},
  { address: 12; id: 7; size: 3}, { address: 15; id: 3; size: 3},
  { address: 15; id: 6; size: 0}, { address: 18; id: 6; size: 1},
  { address: 19; id: 4; size: 2}, { address: 21; id: 6; size: 1},
  { address: 22; id: 5; size: 4}, { address: 26; id: 6; size: 1},
  { address: 27; id: 6; size: 1};
   free_spaces:  }
  0099811188827773336446555566..............
  checksum: 1928 |}];

  Util.with_data_file "day9.txt" (fun ic ->
      IO.read_all ic |> String.trim |> Disk.of_disk_map |> Disk.compact
      |> Disk.checksum |> Util.print_int);
  [%expect {| 6200294120911 |}]

let%expect_test "part2" =
  let disk = Disk.of_disk_map test in
  let disk' = Disk.compact_whole_file disk in

  Format.(
    printf "disk map: %a@.%s@.checksum: %d" Disk.pp disk'
      (Disk.to_disk_map disk') (Disk.checksum disk'));
  [%expect
    {|
    disk map: { files: { address: 0; id: 0; size: 2},
    { address: 2; id: 9; size: 2}, { address: 4; id: 2; size: 1},
    { address: 5; id: 1; size: 3}, { address: 8; id: 7; size: 3},
    { address: 12; id: 4; size: 2}, { address: 15; id: 3; size: 3},
    { address: 22; id: 5; size: 4}, { address: 27; id: 6; size: 4},
    { address: 36; id: 8; size: 4};
     free_spaces: { address: 5; id: -1; size: 0},
    { address: 11; id: -1; size: 0}, { address: 14; id: -1; size: 1},
    { address: 18; id: -1; size: 1}, { address: 21; id: -1; size: 1},
    { address: 26; id: -1; size: 1}, { address: 31; id: -1; size: 1},
    { address: 35; id: -1; size: 1},
    { address: 40; id: -1; size: 0} }
    0099211177744.333..5555.6666..8888........
    checksum: 2858 |}];

  Util.with_data_file "day9.txt" (fun ic ->
      IO.read_all ic |> String.trim |> Disk.of_disk_map
      |> Disk.compact_whole_file |> Disk.checksum |> Util.print_int);
  [%expect {| 6227018762750 |}]
