open! Core

let year = 2023
let day = 2

module Cubes = struct
  type t =
    | Red of int
    | Blue of int
    | Green of int

  let of_split color value =
    match color with
    | "red" -> Red value
    | "blue" -> Blue value
    | "green" -> Green value
    | _ -> failwith "impossible color"
  ;;

  let over_max t =
    match t with
    | Red v -> v > 12
    | Blue v -> v > 14
    | Green v -> v > 13
  ;;
end

module Part_1 = struct
  let run input : (string, string) result =
    let input =
      String.split_lines input
      |> List.map ~f:(fun line -> String.split line ~on:':' |> List.last_exn)
    in
    let round_is_over round =
      String.split ~on:',' round
      |> List.map ~f:(fun r' ->
        String.strip r'
        |> String.split ~on:' '
        |> function
        | [ value; color ] -> Cubes.of_split color (Int.of_string value)
        | _ as failure ->
          failwith
            ([%message "could not parse" (round : string) (failure : string List.t)]
             |> Sexp.to_string))
      |> List.fold ~init:false ~f:(fun acc cube -> acc || Cubes.over_max cube)
    in
    let game_is_over game =
      String.split ~on:';' game
      |> List.fold ~init:false ~f:(fun acc round -> acc || round_is_over round)
    in
    let input = List.map input ~f:game_is_over in
    List.foldi input ~init:0 ~f:(fun idx0 acc game_result ->
      if game_result then acc else acc + idx0 + 1)
    |> Int.to_string
    |> Result.return
  ;;
end

module Part_2 = struct
  let run input : (string, string) result = Ok input
end
