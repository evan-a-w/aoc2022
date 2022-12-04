open Core

let inp = In_channel.create "input/day4.txt"
let ( <| ) f g x = f (g x)

let pairs =
  In_channel.input_lines inp
  |> List.map ~f:(fun x ->
         x |> String.split ~on:','
         |> List.map ~f:(List.map ~f:Int.of_string <| String.split ~on:'-'))

let () =
  let f g = function
    | [ [ a; b ]; [ c; d ] ] -> g a b c d || g c d a b
    | _ -> failwith "bad input"
  in
  let g1 a b c d = c <= a && b <= d in
  let g2 a b c _ = a <= c && c <= b in
  Printf.printf "Part 1: %d\n" @@ (pairs |> List.filter ~f:(f g1) |> List.length);
  Printf.printf "Part 2: %d\n" @@ (pairs |> List.filter ~f:(f g2) |> List.length)
