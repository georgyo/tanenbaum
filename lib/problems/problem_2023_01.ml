open! Core

let year = 2023
let day = 1

module Part_1 = struct
  let run input : (string, string) result =
    let firstlastdigit s =
      String.fold s ~init:(None, None) ~f:(fun (first, last) c ->
        if Char.is_digit c
        then Some (Option.value ~default:c first), Some c
        else first, last)
      |> (function
            | Some f, Some l -> Ok (String.of_char_list [ f; l ])
            | _ -> Result.fail "No digits")
      |> Result.map ~f:Int.of_string
    in
    String.split_lines input
    |> List.fold ~init:0 ~f:(fun acc line ->
      firstlastdigit line |> Result.ok_or_failwith |> Int.( + ) acc)
    |> Int.to_string
    |> Result.return
  ;;
end

module Part_2 = struct
  let run input : (string, string) result = Ok input
end
