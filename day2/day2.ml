open Core
open In_channel
open Base

let file = "./input_1.txt"

let reduce_map (num: int) dict : bool =
        let count = Core.Hashtbl.count dict ~f:( fun a -> equal_int a num) in
        count > 0

let part_one : int =
        let in_chan = create file in
        let input_list = input_lines in_chan in
        let dict = Core.List.map input_list ~f:(fun in_str ->
                Core.List.fold ~init:(Base.Hashtbl.create (module Char))
                ~f:(fun t ele ->
                        Base.Hashtbl.change t ele
                        ~f:(
                                fun o ->
                                        match o with
                                        | None -> Some(1)
                                        | Some(v) -> Some(v+1)
                        );
                        t
                )
                (String.to_list in_str)
        ) in
        let reduced_2 = Core.List.map dict ~f:( (reduce_map 2) ) in
        let reduced_3 = Core.List.map dict ~f:( (reduce_map 3) ) in
        let val_1 = Core.List.fold reduced_2 ~f:(fun acc ele -> (Bool.to_int ele) + acc) ~init:(0) in
        let val_2 = Core.List.fold reduced_3 ~f:(fun acc ele -> (Bool.to_int ele) + acc) ~init:(0) in
        val_1 * val_2

let diff_words (w_1 : string) (w_2 : string) : int =
        let w_1 = String.to_list w_1 in
        let w_2 = String.to_list w_2 in
        match (Core.List.zip w_1 w_2) with
        | Ok(x) -> Core.List.fold x ~init:(0) ~f:(fun acc w -> let (a, b) = w in if (equal_char a b) then acc else acc + 1 )
        | Unequal_lengths -> -1

let rec find_good_pair input_list  (i : int) (j: int) : (string * string) option =
        match Core.List.nth input_list i with
                | None -> None
                | Some(i_ele) ->
                        match Core.List.nth input_list j with
                        | None -> find_good_pair input_list (i+1) 0
                        | Some(j_ele) ->
                                if Int.equal (diff_words i_ele j_ele) 1
                                then Some( (i_ele, j_ele) )
                                else find_good_pair input_list i (j+1)


let strip_diff_chars w_1 w_2 : string =
        let w_1 = String.to_list w_1 in
        let w_2 = String.to_list w_2 in
        match (Core.List.zip w_1 w_2) with
        | Ok(z_words) -> Core.List.fold z_words ~init:("") ~f:( fun acc (a, b) -> if (Char.equal a b) then acc ^ (Char.to_string a) else acc)
        | Unequal_lengths -> ""

let part_two : string =
        let in_chan = create file in
        let input_list = input_lines in_chan in
        match find_good_pair input_list 0 0 with
        | Some( (a, b) ) -> strip_diff_chars a b
        | None -> "Error"


let main =
        printf "return value p1: %d\n" part_one;
        printf "return value p2: %s\n" part_two
