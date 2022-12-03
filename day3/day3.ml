open! Core

let inp () = In_channel.create "input/day3.txt"

let (<|) f g x = f (g x)

let score c = match c with
  | 'a'..'z' -> Char.to_int c - Char.to_int 'a' + 1
  | 'A'..'Z' -> Char.to_int c - Char.to_int 'A' + 27
  | _ -> failwith "invalid character";;

let string_to_set = 
  let accum a = Int.bit_or a <| (Int.shift_left 1) <| score in
  String.fold ~init:0 ~f:accum

let rucksacks =
  In_channel.input_lines @@ inp ()
  |> List.map ~f:(fun line ->
    let len = (String.length line) / 2 in
    let first = String.sub line ~pos:0 ~len in
    let second = String.sub line ~pos:len ~len in
    (string_to_set first, string_to_set second))

let lowest_bit_ind x = 
  let lb = Int.bit_and x (-x) in
  Float.log (Float.of_int lb) /. Float.log 2.0 |> Int.of_float;;

let in_both a b =
  let inter = Int.bit_and a b in
  lowest_bit_ind inter

let threes =
  In_channel.input_lines @@ inp ()
  |> List.map ~f:string_to_set
  |> List.groupi ~break:(fun i _ _ -> i mod 3 = 0)

let in_three a b c =
  let inter = Int.bit_and a (Int.bit_and b c) in
  lowest_bit_ind inter

let () =
  Printf.printf "Part 1: %d\n" @@ List.fold ~init:0 ~f:(fun a (x, y) -> a + in_both x y) rucksacks;
  let foldy_thing a = function
    | [x; y; z] -> a + in_three x y z
    | _ -> failwith "invalid input" in
  Printf.printf "Part 2: %d\n" @@ List.fold ~init:0 ~f:foldy_thing threes

