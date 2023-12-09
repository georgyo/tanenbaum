open! Import

let year = 2023
let day = 9

let test_data = {|0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45|}

let parse input =
  String.split_lines input
  |> List.map ~f:(fun line -> String.split line ~on:' ' |> List.map ~f:Int.of_string)
;;

let fold line =
  let all_zero line = List.fold line ~init:true ~f:(fun acc n -> acc && n = 0) in
  let rec fold ?(acc = []) ~(final_acc : int list) line =
    match line with
    | [] -> raise_s [%message "impossible line" (line : int list)]
    | [ v ] ->
      let final_acc = v :: final_acc in
      if all_zero acc then final_acc |> List.rev else fold ~final_acc (List.rev acc)
    | v1 :: (v2 :: _ as tl) -> fold ~final_acc ~acc:((v2 - v1) :: acc) tl
  in
  fold ~final_acc:[] line
;;

let%expect_test _ =
  parse test_data
  |> List.iter ~f:(fun line ->
    let data = line |> fold in
    print_s [%message (data : int list)]);
  [%expect {|
      (data (15 3))
      (data (21 6 1))
      (data (45 15 6 2)) |}]
;;

module Part_1 = struct
  let run input : string Or_error.t =
    let input = parse input in
    List.map input ~f:fold
    |> List.concat
    |> List.fold ~init:0 ~f:( + )
    |> Int.to_string
    |> return
  ;;

  let%expect_test _ =
    run test_data |> Or_error.ok_exn |> print_endline;
    [%expect "114"]
  ;;
end

module Part_2 = struct
  let run input : string Or_error.t =
    let input = parse input in
    List.map input ~f:(fun line -> List.rev line |> fold)
    |> List.concat
    |> List.fold ~init:0 ~f:( + )
    |> Int.to_string
    |> return
  ;;

  let%expect_test _ =
    run test_data |> Or_error.ok_exn |> print_endline;
    [%expect "2"]
  ;;
end
