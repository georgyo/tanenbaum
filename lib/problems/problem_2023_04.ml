open! Import

let year = 2023
let day = 4

let test_data =
  {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
|}
;;

module Part_1 = struct
  let process_line s =
    let parts = String.split s ~on:':' |> List.last_exn |> String.split ~on:'|' in
    let process_parts l =
      String.split l ~on:' '
      |> List.filter ~f:(fun n -> String.( <> ) n "")
      |> List.map ~f:Int.of_string
      |> Int.Set.of_list
    in
    match List.map parts ~f:process_parts with
    | [ winning_numbers; my_numbers ] -> return (winning_numbers, my_numbers)
    | _ as line ->
      Or_error.error_s [%message "wrong number of parts" (line : Int.Set.t list)]
  ;;

  let run input : string Or_error.t =
    let%map input =
      String.split_lines input |> List.map ~f:process_line |> Or_error.combine_errors
    in
    List.map input ~f:(fun (winning_numbers, my_numbers) ->
      Set.inter winning_numbers my_numbers |> Set.length)
    |> List.fold ~init:0 ~f:(fun acc winning_matches ->
      if winning_matches > 0 then acc + Int.pow 2 (winning_matches - 1) else acc)
    |> Int.to_string
  ;;

  let%expect_test _ =
    run test_data |> Or_error.ok_exn |> print_endline;
    [%expect {| 13 |}]
  ;;
end

module Part_2 = struct
  let run input : string Or_error.t = Ok input

  let%expect_test _ =
    run test_data |> Or_error.ok_exn |> print_endline;
    [%expect
      {|
      Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
      Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
      Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
      Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
      Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
      Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11 |}]
  ;;
end
