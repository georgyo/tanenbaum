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

let process_input input =
  String.split_lines input |> List.map ~f:process_line |> Or_error.combine_errors
;;

module Part_1 = struct
  let score_game (winning_numbers, my_numbers) =
    let winning_matches = Set.inter winning_numbers my_numbers |> Set.length in
    if winning_matches > 0 then Int.pow 2 (winning_matches - 1) else 0
  ;;

  let run input : string Or_error.t =
    let%map input = process_input input in
    List.map input ~f:score_game |> List.fold ~init:0 ~f:( + ) |> Int.to_string
  ;;

  let%expect_test _ =
    run test_data |> Or_error.ok_exn |> print_endline;
    [%expect {| 13 |}]
  ;;
end

module Part_2 = struct
  let incr_dup_card list by count =
    let make_new_list by count = List.init count ~f:(fun _ -> by) in
    let rec f acc l1 l2 =
      match l1, l2 with
      | a :: tl1, b :: tl2 -> f ((a + b) :: acc) tl1 tl2
      | [], [] -> acc
      | hd :: tl, [] | [], hd :: tl -> f (hd :: acc) tl []
    in
    f [] (make_new_list by count) list |> List.rev
  ;;

  let score_game (winning_numbers, my_numbers) =
    Set.inter winning_numbers my_numbers |> Set.length
  ;;

  let run input : string Or_error.t =
    let%map input = process_input input in
    let rec helper ~acc = function
      | [], _ -> acc
      | game :: gtl, copies :: ctl ->
        let copies = copies + 1 in
        let score = score_game game in
        let ctl = incr_dup_card ctl copies score in
        print_s [%message (score : int) (copies : int)];
        helper ~acc:(acc + copies) (gtl, ctl)
      | game :: gtl, ([] as ctl) ->
        let copies = 1 in
        let score = score_game game in
        let ctl = incr_dup_card ctl copies score in
        print_s [%message (score : int) (copies : int)];
        helper ~acc:(acc + copies) (gtl, ctl)
    in
    helper ~acc:0 (input, []) |> Int.to_string
  ;;

  let%expect_test _ =
    run test_data |> Or_error.ok_exn |> print_endline;
    [%expect
      {|
      ((score 4) (copies 1))
      ((score 2) (copies 2))
      ((score 2) (copies 4))
      ((score 1) (copies 8))
      ((score 0) (copies 14))
      ((score 0) (copies 1))
      30 |}]
  ;;
end
