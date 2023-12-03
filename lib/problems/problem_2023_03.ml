open! Import

let year = 2023
let day = 3

module Part_1 = struct
  let run input : string Or_error.t =
    let input =
      String.split_lines input |> List.map ~f:String.to_array |> List.to_array
    in
    let len_x = Array.length input.(0) in
    let len_y = Array.length input in
    let surrondings cords =
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
    let search_surroundings x y =
      let value =
        surrondings
          [ x - 1, y - 1
          ; x - 1, y
          ; x - 1, y + 1
          ; x, y - 1
          ; x, y + 1
          ; x + 1, y - 1
          ; x + 1, y
          ; x + 1, y + 1
          ]
      in
      value
    in
    let tmp_space = ref [] in
    let tmp_valid = ref false in
    let reset () =
      tmp_space := [];
      tmp_valid := false
    in
    let reset_and_return ret =
      reset ();
      ret
    in
    let int_of_chart_list clist =
      List.rev clist |> String.of_char_list |> Int.of_string
    in
    let valid_numbers =
      Array.foldi input ~init:[] ~f:(fun y (acc : Int.t List.t) yv ->
        print_endline [%string "Processing line %{y#Int}"];
        let line_valid_numbers =
          Array.foldi ~init:[] yv ~f:(fun x acc xv ->
            match xv with
            | '0' .. '9' ->
              tmp_space := xv :: !tmp_space;
              tmp_valid := search_surroundings x y || !tmp_valid;
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
  let run input : string Or_error.t = Ok input
end
