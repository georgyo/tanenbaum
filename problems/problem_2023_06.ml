open! Import

let year = 2023
let day = 6

let test_data = {|Time:      7  15   30
Distance:  9  40  200|}

module Part_1 = struct
  let run input : string Or_error.t =
    let input =
      String.split_lines input
      |> List.map ~f:(fun line ->
        String.split line ~on:':'
        |> List.last_exn
        |> String.split ~on:' '
        |> List.filter ~f:(String.( <> ) "")
        |> List.map ~f:Int.of_string)
    in
    let%map input =
      match input with
      | [ time; distance ] -> List.zip_exn time distance |> return
      | _ -> Or_error.error_s [%message "unknown input" (input : int list list)]
    in
    let can_win ~speed time distance = speed * (time - speed) > distance in
    let rec min_max ~time ~distance ~speed (min, max) =
      if speed >= time
      then min, max
      else
        min_max
          ~time
          ~distance
          ~speed:(speed + 1)
          (if can_win ~speed time distance
           then Option.value ~default:speed min |> Option.return, Option.return speed
           else min, max)
    in
    List.map input ~f:(fun (time, distance) ->
      match min_max ~time ~distance ~speed:1 (None, None) with
      | Some min, Some max -> max - min + 1
      | _, _ ->
        raise_s [%message "could not find min max" (time : Int.t) (distance : Int.t)])
    |> List.fold ~init:1 ~f:( * )
    |> Int.to_string
  ;;

  (* [%message (input : (int * int) list Or_error.t)] |> Sexp.to_string_hum |> return *)

  let%expect_test _ =
    run test_data |> Or_error.ok_exn |> print_endline;
    [%expect {| 288 |}]
  ;;
end

module Part_2 = struct
  let run input : string Or_error.t =
    let input =
      String.split_lines input
      |> List.map ~f:(fun line ->
        String.split line ~on:':'
        |> List.last_exn
        |> String.filter ~f:Char.is_digit
        |> Int.of_string)
    in
    let%map time, distance =
      match input with
      | [ time; distance ] -> (time, distance) |> return
      | _ -> Or_error.error_s [%message "unknown input" (input : int list)]
    in
    let can_win ~speed time distance = speed * (time - speed) > distance in
    let rec min_max ~time ~distance ~speed (min, max) =
      if speed >= time
      then min, max
      else
        min_max
          ~time
          ~distance
          ~speed:(speed + 1)
          (if can_win ~speed time distance
           then Option.value ~default:speed min |> Option.return, Option.return speed
           else min, max)
    in
    (match min_max ~time ~distance ~speed:1 (None, None) with
     | Some min, Some max -> max - min + 1
     | _, _ ->
       raise_s [%message "could not find min max" (time : Int.t) (distance : Int.t)])
    |> Int.to_string
  ;;

  let%expect_test _ =
    run test_data |> Or_error.ok_exn |> print_endline;
    [%expect {| 71503 |}]
  ;;
end
