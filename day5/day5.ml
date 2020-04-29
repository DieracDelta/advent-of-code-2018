open Core
open In_channel
open Base

let file = "./input_1.txt"


type modulo = Zero | One

let remove_char input ~ele:(ele) =
  let rgx = Str.regexp ele in
  Str.split rgx input |>
  List.fold ~init:("") ~f:(fun acc ele -> acc ^ ele)

let remove_up_low input ele =
  remove_char input ~ele:(String.lowercase ele) |> remove_char ~ele:(String.uppercase ele)

(*stolen from https://rosettacode.org/wiki/Generate_lower_case_ASCII_alphabet#OCaml*)
let rec gen_alphabet l cur_char =
  if Int.equal (List.length l) 26 then l else
    let new_ele = Char.to_int cur_char |> ( ( + ) 1) |> Char.of_int_exn in
    gen_alphabet (List.append l [new_ele]) new_ele

(*checks if two consecutive characters are conductive*)
let is_conductive a b =
  let l_a = Base.Char.lowercase a in
  let l_b = Base.Char.lowercase b in
  (Base.Char.(=) l_a l_b) && (Base.Char.(=) a b |> not)

let reduce_by_conductivity (offset : bool) (l: char Core.Fdeque.t ) : char Core.Fdeque.t =
  let (acc, l) = if offset then let (hd, rest) = Core.Fdeque.dequeue_back_exn l
      in (Core.Fdeque.enqueue_front Core.Fdeque.empty hd, rest) else (Core.Fdeque.empty, l) in
  let rq = Core.Fdeque.rev l in
  Core.Fdeque.fold rq ~init:((acc, Zero)) ~f:(fun (acc, modulo) ele ->
      (*let _ = printf "\n %s" (Char.to_string ele) in*)
      match modulo with
      | Zero -> (Core.Fdeque.enqueue_front acc ele, One)
      | One ->
        let (prev_ele, rest) = Core.Fdeque.dequeue_front_exn acc in
        if is_conductive prev_ele ele then
          (*let _ = printf "reduced! %s %s" (Char.to_string prev_ele) (Char.to_string ele) in*)
          (rest, Zero)
        else (Core.Fdeque.enqueue_front acc ele, Zero)
    )
  |> fst

let rec outer_loop l =
  let orig_len = Core.Fdeque.length l in
  let reduced = reduce_by_conductivity false l |> reduce_by_conductivity true in
  if orig_len > Core.Fdeque.length reduced then outer_loop reduced else l

let list_to_queue l =
  l |> List.fold ~init:(Core.Fdeque.empty) ~f:(fun acc ele -> Core.Fdeque.enqueue_front acc ele)

let get_reduced_len (input: string) : int =
  input |> String.to_list |> list_to_queue |> outer_loop |>  Core.Fdeque.length

let part_one : int =
  match create file |> Stdio.In_channel.input_line with
  | None -> -1
  | Some(x) -> x |> get_reduced_len



let part_two : int =
  match create file |> Stdio.In_channel.input_line with
  | None -> -1
  | Some(input) ->
    (gen_alphabet [] 'a') |> List.map  ~f:(fun ele -> Char.to_string ele |> remove_up_low input |> get_reduced_len)
    |> List.fold ~init:(Int.max_value) ~f:(fun acc ele -> if acc > ele then ele else acc)

let main =
  printf "\nreturn value p1: %d\n" part_one;
  printf "\nreturn value p2: %d\n" part_two;
