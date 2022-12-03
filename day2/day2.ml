open! Core

let inp = In_channel.create "input/day2.txt"

module RPS = struct
  type t = Rock | Paper | Scissors

  let of_string = function
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> failwith "Invalid input"

  let int = function Rock -> 1 | Paper -> 2 | Scissors -> 3

  let win a b =
    match (a, b) with
    | Scissors, Rock | Rock, Paper | Paper, Scissors -> 2
    | _ when int a = int b -> 1
    | _ -> 0

  let to_win = function Rock -> Paper | Paper -> Scissors | Scissors -> Rock
  let to_lose = function Rock -> Scissors | Paper -> Rock | Scissors -> Paper
  let score_match a b = int b + (win a b * 3)
end

let matches =
  In_channel.input_lines inp
  |> List.map ~f:(Fn.compose (List.map ~f:RPS.of_string) (String.split ~on:' '))

let () =
  Printf.printf "Part 1: %d\n"
  @@ List.sum
       (module Int)
       matches
       ~f:(function [ a; b ] -> RPS.score_match a b | _ -> failwith "fk u");
  Printf.printf "Part 2: %d\n"
  @@ List.sum
       (module Int)
       matches
       ~f:(function
         | [ a; b ] -> (
             match b with
             | RPS.Rock -> RPS.score_match a (RPS.to_lose a)
             | RPS.Paper -> RPS.score_match a a
             | RPS.Scissors -> RPS.score_match a (RPS.to_win a))
         | _ -> failwith "fk u")
