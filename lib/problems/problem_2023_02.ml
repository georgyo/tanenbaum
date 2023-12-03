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

  let value t =
    match t with
    | Red v | Blue v | Green v -> v
  ;;

  let of_game input =
    let round_spit round =
      String.split ~on:',' round
      |> List.map ~f:(fun r' ->
        String.strip r'
        |> String.split ~on:' '
        |> function
        | [ value; color ] -> of_split color (Int.of_string value)
        | _ as failure ->
          failwith
            ([%message "could not parse" (round : string) (failure : string List.t)]
             |> Sexp.to_string))
    in
    String.split ~on:';' input |> List.map ~f:round_spit
  ;;

  let over_max t =
    match t with
    | Red v -> v > 12
    | Blue v -> v > 14
    | Green v -> v > 13
  ;;

  let max tlist =
    List.fold tlist ~init:(Red 1, Blue 1, Green 1) ~f:(fun (red, blue, green) t ->
      (match t with
       | Red v as red' -> Option.some_if (v > value red) (red', blue, green)
       | Blue v as blue' -> Option.some_if (v > value blue) (red, blue', green)
       | Green v as green' -> Option.some_if (v > value green) (red, blue, green'))
      |> Option.value ~default:(red, blue, green))
  ;;
end

module Part_1 = struct
  let run input : (string, string) result =
    let input =
      String.split_lines input
      |> List.map ~f:(fun line -> String.split line ~on:':' |> List.last_exn)
    in
    let games =
      List.map input ~f:(fun game ->
        Cubes.of_game game
        |> List.fold ~init:false ~f:(fun acc round ->
          acc
          || List.fold round ~init:false ~f:(fun acc color -> acc || Cubes.over_max color)))
    in
    List.foldi games ~init:0 ~f:(fun idx0 acc game_result ->
      if game_result then acc else acc + idx0 + 1)
    |> Int.to_string
    |> Result.return
  ;;
end

module Part_2 = struct
  let run input : (string, string) result =
    let input =
      String.split_lines input
      |> List.map ~f:(fun line -> String.split line ~on:':' |> List.last_exn)
    in
    let game_maxs =
      List.map input ~f:(fun game -> Cubes.of_game game |> List.concat |> Cubes.max)
    in
    List.fold game_maxs ~init:0 ~f:(fun acc (red, blue, green) ->
      acc + Cubes.(value red * value blue * value green))
    |> Int.to_string
    |> Result.return
  ;;
end
