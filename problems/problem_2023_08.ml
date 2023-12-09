open! Import

let year = 2023
let day = 8

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

module Part_1 = struct
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
    {|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)|}
    |> run
    |> Or_error.ok_exn
    |> print_endline;
    [%expect {|2|}]
  ;;

  let%expect_test "b" =
    {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)|}
    |> run
    |> Or_error.ok_exn
    |> print_endline;
    [%expect {|6|}]
  ;;
end

module Part_2 = struct
  let run input : string Or_error.t =
    let moves, map = parse input in
    let starting_locs =
      Map.keys map |> List.filter ~f:(StringLabels.ends_with ~suffix:"A")
    in
    let move ~(loc : string) ~(direction : Direction.t) =
      match direction, Map.find_exn map loc with
      | L, (next, _) | R, (_, next) -> next
    in
    let rec f ~count ~(loc : string) moves' =
      let call ~direction tl : int =
        let next = move ~direction ~loc in
        match tl with
        | [] -> f ~count:(count + 1) ~loc:next moves
        | tl -> f ~count:(count + 1) ~loc:next tl
      in
      if StringLabels.ends_with ~suffix:"Z" loc
      then count
      else (
        match moves' with
        | direction :: tl -> call ~direction tl
        | [] -> raise_s [%message "impossible" [%here]])
    in
    List.map starting_locs ~f:(fun loc ->
      let distance = f ~loc ~count:0 moves in
      print_s [%message (loc : string) (distance : int)];
      distance)
    |> List.fold ~init:1 ~f:( * )
    |> Int.to_string
    |> return
  ;;

  let%expect_test _ =
    {|LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)|}
    |> run
    |> Or_error.ok_exn
    |> print_endline;
    [%expect {|
      ((loc 11A) (distance 2))
      ((loc 22A) (distance 3))
      6|}]
  ;;
end
