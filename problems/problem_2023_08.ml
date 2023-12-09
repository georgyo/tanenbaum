open! Import

let year = 2023
let day = 8

let test_data_a =
  {|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)|}
;;

let test_data_b = {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)|}

module Direction = struct
  type t =
    | L
    | R

  let of_char = function
    | 'L' -> L
    | 'R' -> R
    | _ -> raise_s [%message "invalid"]
  ;;
end

module Part_1 = struct
  let parse input =
    match String.split_lines input with
    | [] -> raise_s [%message "impossible" (input : string)]
    | hd :: tl ->
      ( String.to_list hd |> List.map ~f:Direction.of_char
      , List.map tl ~f:(fun line ->
          String.split ~on:'=' line
          |> List.map ~f:(fun parts ->
            String.strip parts ~drop:(function
              | ' ' | '(' | ')' -> true
              | _ -> false)
            |> String.split ~on:','
            |> List.map ~f:String.strip))
        |> List.filter_map ~f:(function
          | [ [ "" ] ] -> None
          | [ [ node ]; [ left; right ] ] -> Some (node, (left, right))
          | _ -> raise_s [%message "impossible" [%here]])
        |> String.Map.of_alist_exn )
  ;;

  let run input : string Or_error.t =
    let moves, map = parse input in
    let rec f ~count loc moves' =
      let call tl next =
        match tl with
        | [] -> f ~count:(count + 1) next moves
        | tl -> f ~count:(count + 1) next tl
      in
      match loc with
      | "ZZZ" -> count
      | _ ->
        (match moves', Map.find_exn map loc with
         | L :: tl, (next, _) | R :: tl, (_, next) -> call tl next
         | [], _ -> raise_s [%message "impossible" [%here]])
    in
    f ~count:0 "AAA" moves |> Int.to_string |> return
  ;;

  let%expect_test "a" =
    run test_data_a |> Or_error.ok_exn |> print_endline;
    [%expect {|2|}]
  ;;

  let%expect_test "b" =
    run test_data_b |> Or_error.ok_exn |> print_endline;
    [%expect {|6|}]
  ;;
end

module Part_2 = struct
  let run _input : string Or_error.t = Ok ""

  let%expect_test _ =
    run test_data_a |> Or_error.ok_exn |> print_endline;
    [%expect {||}]
  ;;
end
