open Core
open In_channel
open Base

let file = "./input_1.txt"

module IntTuple = struct
        type t = (int * int * int) [@@deriving sexp]
        (*it's easiest just to spell out that the function is reflexive in order to get a total ordering*)
        (*no idea why this required for a set though... you should only really need to compare for equality*)
        let compare = (fun (_, a, b) (_, c, d) ->
                if Int.equal a c && Int.equal b d then 0
                else
                        if a > c && Int.equal b d then 1
                        else
                        if a < c && Int.equal b d then -1
                        else if Int.equal a c && b > d then 1
                        else if a > c && b > d then 1
                        else if a < c && b > d then -1
                        else if Int.equal a c && b < d then -1
                        else if a > c && b < d then 1
                        else -1
        )
end

module IntTupleSet = Core.Set.Make(IntTuple);;


let gen_tuples offset_x offset_y width height id: IntTupleSet.t =
        let x_range = List.range ~start:(`exclusive) ~stop:(`inclusive) offset_x (offset_x + width) in
        let y_range = List.range ~start:(`exclusive) ~stop:(`inclusive) offset_y (offset_y + height) in
        let cart_prod = List.cartesian_product x_range y_range in
        List.fold cart_prod ~init:(IntTupleSet.empty)
                ~f:( fun acc (a , b) -> IntTupleSet.union acc (IntTupleSet.singleton (id, a, b)))


let parse_params (in_str: string) : IntTupleSet.t =
        let rgx = Str.regexp "[^0-9]+" in
        let applied_rgx = Str.split rgx in_str in
        let parsed_ints = List.map applied_rgx ~f:(Int.of_string) in
        match parsed_ints with
        | [id; offset_x; offset_y; width; height] -> gen_tuples offset_x offset_y width height id
        | _ -> IntTupleSet.empty

let part_one : int =
        let in_chan = create file in
        let input_list = input_lines in_chan in
        let r_sets = List.map input_list ~f:(parse_params) in
        let cart_prod = List.cartesian_product r_sets r_sets in
        let reduced = List.map cart_prod ~f:(fun (a, b) ->
                if IntTupleSet.equal a b
                then
                        IntTupleSet.empty
                else
                        IntTupleSet.inter a b
        ) in
        let fin = List.fold reduced ~init:(IntTupleSet.empty) ~f:(IntTupleSet.union) in
        (*let _ = IntTupleSet.iter fin ~f:(fun (a, b) -> printf "\n(%d, %d)\n" a b) in*)
        IntTupleSet.length fin

let part_two : int =
        let in_chan = create file in
        let input_list = input_lines in_chan in
        let r_sets = List.map input_list ~f:(parse_params) in
        match List.findi r_sets ~f:(fun index elf_list ->
               List.foldi r_sets ~init:(true) ~f:(fun index_2 acc sec_elf_list ->
                       if Int.equal index index_2 |> not then
                        acc && IntTupleSet.inter elf_list sec_elf_list |> IntTupleSet.is_empty
                        else acc
                )
        ) with
        | None -> -1
        | Some(_, x) -> match List.random_element (IntTupleSet.elements x) with
                | None -> -2
                | Some((id, _, _)) -> id

let main =
        printf "\nreturn value p1: %d\n" part_one;
        printf "\nreturn value p2: %d\n" part_two;
