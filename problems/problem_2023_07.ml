open! Import

let year = 2023
let day = 7

let test_data = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

module Card = struct
  module T = struct
    type t = char [@@deriving sexp]

    let card_value card =
      match card with
      | 'X' -> 1 (* Wild card replacement *)
      | '2' .. '9' -> String.of_char card |> Int.of_string
      | 'T' -> 10
      | 'J' -> 11
      | 'Q' -> 12
      | 'K' -> 13
      | 'A' -> 14
      | _ -> raise_s [%message "invalid card" (card : Char.t)]
    ;;

    let compare t1 t2 = card_value t1 - card_value t2
  end

  include T
  include Comparable.Make (T)

  let%expect_test _ =
    'A' > 'K' |> Bool.to_string |> print_endline;
    [%expect "true"]
  ;;
end

module Hand_type = struct
  module T = struct
    type t =
      | High
      | One
      | Two
      | Three
      | Full
      | Four
      | Five
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let%expect_test _ =
    Five > High |> Bool.to_string |> print_endline;
    [%expect "true"]
  ;;
end

module Hand = struct
  module T = struct
    type t =
      { hand : Card.t List.t
      ; bid : int
      }
    [@@deriving sexp]

    let to_hand_type (t : t) =
      let hand = List.sort t.hand ~compare:Card.compare in
      let jokers = ref 0 in
      let rec matches ~acc ~prev ~prev_count hand =
        match hand with
        | [] -> if prev_count > 1 then prev_count :: acc else acc
        | 'X' :: tl ->
          jokers := !jokers + 1;
          matches ~acc ~prev ~prev_count tl
        | c :: tl when Card.(c = prev) ->
          matches ~acc ~prev ~prev_count:(prev_count + 1) tl
        | c :: tl ->
          matches
            ~acc:(if prev_count > 1 then prev_count :: acc else acc)
            ~prev:c
            ~prev_count:1
            tl
      in
      let matches =
        matches ~acc:[] ~prev:'2' ~prev_count:0 hand
        |> List.sort ~compare:Int.compare
        |> List.rev
      in
      let matches =
        match matches with
        | [] -> if !jokers > 1 then [ !jokers ] else matches
        | hd :: tl -> (hd + !jokers) :: tl
      in
      match matches with
      | [ 5 ] -> Hand_type.Five
      | [ 4 ] -> Hand_type.Four
      | [ 3; 2 ] -> Hand_type.Full
      | [ 3 ] -> Hand_type.Three
      | [ 2; 2 ] -> Hand_type.Two
      | [ 2 ] -> Hand_type.One
      | [] -> Hand_type.High
      | _ ->
        raise_s [%message "Impossible hand" (hand : Card.t list) (matches : int list)]
    ;;

    let compare t1 t2 : int =
      let hand_compare = Hand_type.compare (to_hand_type t1) (to_hand_type t2) in
      let rec compare_cards t1 t2 =
        match t1, t2 with
        | [], [] -> 0
        | c1 :: _, c2 :: _ when Card.compare c1 c2 <> 0 -> Card.compare c1 c2
        | _ :: tl1, _ :: tl2 -> compare_cards tl1 tl2
        | _, _ -> raise_s [%message "impossible" (t1 : Card.t list) (t2 : Card.t list)]
      in
      if hand_compare = 0 then compare_cards t1.hand t2.hand else hand_compare
    ;;
  end

  include T
  include Comparable.Make (T)

  let of_string ?wild s =
    let s =
      match wild with
      | Some wild -> String.map s ~f:(fun c -> if Char.(c = wild) then 'X' else c)
      | None -> s
    in
    match String.split ~on:' ' s with
    | [ hand; bid ] -> { hand = String.to_list hand; bid = Int.of_string bid }
    | _ -> raise_s [%message "invalid hand" (s : string)]
  ;;

  let%expect_test _ =
    let hand1 = of_string "T55J5 10" in
    let hand2 = of_string "KK677 10" in
    hand1 > hand2 |> Bool.to_string |> print_endline;
    [%expect "true"]
  ;;
end

module Part_1 = struct
  let run input : string Or_error.t =
    String.split_lines input
    |> List.map ~f:Hand.of_string
    |> List.sort ~compare:Hand.compare
    |> List.foldi ~init:0 ~f:(fun i acc { bid; _ } -> acc + (bid * (i + 1)))
    |> Int.to_string
    |> return
  ;;

  let%expect_test "part1_value" =
    run test_data |> Or_error.ok_exn |> print_endline;
    [%expect {|6440|}]
  ;;
end

module Part_2 = struct
  let run input : string Or_error.t =
    String.split_lines input
    |> List.map ~f:(Hand.of_string ~wild:'J')
    |> List.sort ~compare:Hand.compare
    |> List.foldi ~init:0 ~f:(fun i acc { bid; _ } -> acc + (bid * (i + 1)))
    |> Int.to_string
    |> return
  ;;

  let%expect_test "part2_value" =
    run test_data |> Or_error.ok_exn |> print_endline;
    [%expect {|5905|}]
  ;;
end
