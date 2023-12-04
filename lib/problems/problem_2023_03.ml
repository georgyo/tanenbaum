open! Import

let year = 2023
let day = 3

let search_surroundings ?init ~f x y =
  f
    ?init
    [ x - 1, y - 1
    ; x - 1, y
    ; x - 1, y + 1
    ; x, y - 1
    ; x, y + 1
    ; x + 1, y - 1
    ; x + 1, y
    ; x + 1, y + 1
    ]
;;

let int_of_chart_list clist = List.rev clist |> String.of_char_list |> Int.of_string
let tmp_space = ref []
let tmp_valid = ref false
let tmp_cords = ref None

let reset () =
  tmp_space := [];
  tmp_valid := false;
  tmp_cords := None
;;

let reset_and_return ret =
  reset ();
  ret
;;

module Cords = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, hash, sexp]

    let of_xy ~x ~y : t = x, y
  end

  include T
  module Map = Map.Make (T)
end

module Part_1 = struct
  let run input : string Or_error.t =
    let input =
      String.split_lines input |> List.map ~f:String.to_array |> List.to_array
    in
    let len_x = Array.length input.(0) in
    let len_y = Array.length input in
    let surrondings ?init cords =
      ignore init;
      List.fold
        ~init:false
        ~f:(fun acc (x, y) ->
          match x, y with
          | -1, _ | _, -1 -> acc
          | x', y' when x' >= len_x || y' >= len_y -> acc
          | x', y' ->
            (match input.(y').(x') with
             | '.' | '0' .. '9' -> acc || false
             | _ -> acc || true))
        cords
    in
    let valid_numbers =
      Array.foldi input ~init:[] ~f:(fun y (acc : Int.t List.t) yv ->
        print_endline [%string "Processing line %{y#Int}"];
        let line_valid_numbers =
          Array.foldi ~init:[] yv ~f:(fun x acc xv ->
            match xv with
            | '0' .. '9' ->
              tmp_space := xv :: !tmp_space;
              tmp_valid := search_surroundings ~f:surrondings x y || !tmp_valid;
              acc
            | _ ->
              reset_and_return
                (if Ref.( ! ) tmp_valid && not (List.is_empty !tmp_space)
                 then (
                   let number = Ref.( ! ) tmp_space |> int_of_chart_list in
                   number :: acc)
                 else acc))
        in
        let acc = List.concat [ line_valid_numbers; acc ] in
        reset_and_return
          (if Ref.( ! ) tmp_valid && not (List.is_empty !tmp_space)
           then (
             let number = !tmp_space |> int_of_chart_list in
             number :: acc)
           else acc))
    in
    List.fold ~init:0 ~f:( + ) valid_numbers |> Int.to_string |> return
  ;;
end

module Part_2 = struct
  let run input : string Or_error.t =
    let input =
      String.split_lines input |> List.map ~f:String.to_array |> List.to_array
    in
    let len_x = Array.length input.(0) in
    let len_y = Array.length input in
    let surrondings ?init cords =
      List.fold
        ~init
        ~f:(fun acc (x, y) ->
          match x, y with
          | -1, _ | _, -1 -> acc
          | x', y' when x' >= len_x || y' >= len_y -> acc
          | x', y' ->
            (match input.(y').(x') with
             | '*' -> Some (x', y')
             | _ -> acc))
        cords
    in
    let valid_numbers =
      Array.foldi input ~init:[] ~f:(fun y acc yv ->
        print_endline [%string "Processing line %{y#Int}"];
        let line_valid_numbers =
          Array.foldi ~init:[] yv ~f:(fun x acc xv ->
            match xv with
            | '0' .. '9' ->
              tmp_space := xv :: !tmp_space;
              tmp_cords := search_surroundings ?init:!tmp_cords ~f:surrondings x y;
              acc
            | _ ->
              reset_and_return
                (match !tmp_cords, not (List.is_empty !tmp_space) with
                 | Some (x', y'), true ->
                   let number = Ref.( ! ) tmp_space |> int_of_chart_list in
                   (Cords.of_xy ~x:x' ~y:y', number) :: acc
                 | _, _ -> acc))
        in
        let acc = List.concat [ line_valid_numbers; acc ] in
        reset_and_return
          reset_and_return
          (match !tmp_cords, not (List.is_empty !tmp_space) with
           | Some (x', y'), true ->
             let number = Ref.( ! ) tmp_space |> int_of_chart_list in
             (Cords.of_xy ~x:x' ~y:y', number) :: acc
           | _, _ -> acc))
    in
    let number_map = Cords.Map.of_alist_multi valid_numbers in
    Map.fold number_map ~init:0 ~f:(fun ~key ~data acc ->
      ignore key;
      match data with
      | [ v1; v2 ] -> (v1 * v2) + acc
      | _ -> acc)
    |> Int.to_string
    |> return
  ;;
end
