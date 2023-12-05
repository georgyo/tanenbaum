open! Import

let year = 2023
let day = 5

let test_data =
  {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|}
;;

module Input_data = struct
  module DstSrcMap = struct
    type t =
      { dst : int
      ; src : int
      ; range : int
      }
    [@@deriving sexp, fields]

    let find_next_map (t : t list) input =
      let map =
        List.find t ~f:(fun { dst = _; src; range } ->
          Int.between ~low:src ~high:(src + range) input)
      in
      match map with
      | Some { dst; src; _ } -> dst - src + input
      | None -> input
    ;;

    let find_last_value (t : t list list) input =
      List.fold t ~init:input ~f:(fun acc maps -> find_next_map maps acc)
    ;;
  end

  type t =
    { seeds : int list
    ; maps : DstSrcMap.t list list
    }
  [@@deriving sexp]

  let parse input : t =
    let input =
      String.split_lines input
      |> List.map ~f:(fun line ->
        match String.substr_index line ~pattern:"map" with
        | Some _ -> "map:"
        | None -> line)
    in
    let seeds = ref None in
    let maps : DstSrcMap.t List.t List.t Ref.t = ref [] in
    let tmp : DstSrcMap.t List.t Ref.t = ref [] in
    let () =
      List.iter input ~f:(fun line ->
        String.split ~on:' ' line
        |> function
        | "seeds:" :: tl -> seeds := Some (List.map tl ~f:Int.of_string)
        | "map:" :: _ -> tmp := []
        | "" :: _ -> if not (List.is_empty !tmp) then maps := List.rev !tmp :: !maps
        | [ dst; src; range ] ->
          tmp
          := DstSrcMap.Fields.create
               ~dst:(Int.of_string dst)
               ~src:(Int.of_string src)
               ~range:(Int.of_string range)
             :: !tmp
        | _ -> raise_s [%message "Invalid input line" (line : string)])
    in
    let () = if not (List.is_empty !tmp) then maps := List.rev !tmp :: !maps in
    { seeds = Option.value_exn !seeds; maps = List.rev !maps }
  ;;

  let%expect_test _ =
    parse test_data |> sexp_of_t |> print_s;
    [%expect
      {|
      ((seeds (79 14 55 13))
       (maps
        ((((dst 50) (src 98) (range 2)) ((dst 52) (src 50) (range 48)))
         (((dst 0) (src 15) (range 37)) ((dst 37) (src 52) (range 2))
          ((dst 39) (src 0) (range 15)))
         (((dst 49) (src 53) (range 8)) ((dst 0) (src 11) (range 42))
          ((dst 42) (src 0) (range 7)) ((dst 57) (src 7) (range 4)))
         (((dst 88) (src 18) (range 7)) ((dst 18) (src 25) (range 70)))
         (((dst 45) (src 77) (range 23)) ((dst 81) (src 45) (range 19))
          ((dst 68) (src 64) (range 13)))
         (((dst 0) (src 69) (range 1)) ((dst 1) (src 0) (range 69)))
         (((dst 60) (src 56) (range 37)) ((dst 56) (src 93) (range 4)))))) |}]
  ;;
end

module Part_1 = struct
  let run input : string Or_error.t =
    let input = Input_data.parse input in
    List.map input.seeds ~f:(Input_data.DstSrcMap.find_last_value input.maps)
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
    |> Int.to_string
    |> return
  ;;

  let%expect_test _ =
    run test_data |> Or_error.ok_exn |> print_endline;
    [%expect {|
      35 |}]
  ;;
end

module Part_2 = struct
  let run _input : string Or_error.t = Ok "input"

  let%expect_test _ =
    run test_data |> Or_error.ok_exn |> print_endline;
    [%expect {| input |}]
  ;;
end
