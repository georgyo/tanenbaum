open! Core
open! Or_error.Let_syntax

let year = 2023
let day = 1

module Part_1 = struct
  let run input : string Or_error.t =
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
    |> return
  ;;
end

module Part_2 = struct
  let rec firstlastdigit (first, last) clist =
    let helper n tl = firstlastdigit (Some (Option.value ~default:n first), Some n) tl in
    match clist with
    | [] -> first, last
    | ('1' .. '9' as n) :: tl -> helper n tl
    | 'o' :: ('n' :: 'e' :: _ as tl) -> helper '1' tl
    | 't' :: ('w' :: 'o' :: _ as tl) -> helper '2' tl
    | 't' :: ('h' :: 'r' :: 'e' :: 'e' :: _ as tl) -> helper '3' tl
    | 'f' :: ('o' :: 'u' :: 'r' :: _ as tl) -> helper '4' tl
    | 'f' :: ('i' :: 'v' :: 'e' :: _ as tl) -> helper '5' tl
    | 's' :: ('i' :: 'x' :: _ as tl) -> helper '6' tl
    | 's' :: ('e' :: 'v' :: 'e' :: 'n' :: _ as tl) -> helper '7' tl
    | 'e' :: ('i' :: 'g' :: 'h' :: 't' :: _ as tl) -> helper '8' tl
    | 'n' :: ('i' :: 'n' :: 'e' :: _ as tl) -> helper '9' tl
    | _ :: tl -> firstlastdigit (first, last) tl
  ;;

  let firstlastdigit clist =
    firstlastdigit (None, None) clist
    |> function
    | Some f, Some l -> Some (String.of_char_list [ f; l ] |> Int.of_string)
    | _ -> None
  ;;

  let run input : string Or_error.t =
    String.split_lines input
    |> List.map ~f:String.to_list
    |> List.filter_map ~f:firstlastdigit
    |> List.fold ~init:0 ~f:( + )
    |> Int.to_string
    |> return
  ;;
end
