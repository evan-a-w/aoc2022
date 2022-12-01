open! Core

let inp = In_channel.create "input/day1.txt"

let calories =
  let accum (small, big) line =
    if String.equal line "" then
      (0, small :: big)
    else
      (Int.of_string line + small, big) in
  In_channel.fold_lines inp ~init:(0, []) ~f:accum
    |> fun (a, b) -> a :: b

let () =
  Printf.printf "Part 1: %d\n" (List.max_elt ~compare:Int.compare calories |> Option.value_exn);
  Printf.printf
    "Part 2: %d\n"
    (calories
      |> List.sort ~compare:(Fn.flip Int.compare)
      |> (Fn.flip List.take) 3
      |> List.fold ~init:0 ~f:(+))

