open Core
open In_channel

let file = "./input_1.txt"

module IntSet = Set.Make(Int);;


let accumulate_int (a : int) (b : string) : int =
        a + (Int.of_string b)

let part_one : int =
        let in_chan = create file in
        let r_val = fold_lines in_chan ~init:(0) ~f:(accumulate_int) in
        close in_chan; r_val
        (*Printf.printf "return value: %f\n" r_val*)

let rec find_repeat input_list acc: int =
        let freq, seen, finished = Core.List.fold_until ~init:acc
        ~f:(fun acc ele ->
                let freq, ss, _ = acc in
                let new_freq = freq + ele in
                        if IntSet.mem ss new_freq
                        then
                                Stop (new_freq, ss, true)
                        else
                                Continue (new_freq, IntSet.union ss (IntSet.singleton new_freq), false )
        )
        ~finish:(fun acc -> acc)
        input_list
        in
                if finished
                then freq
                else find_repeat input_list (freq, seen, false)


let part_two : int =
        let in_chan = create file in
        let in_list = Core.List.map (input_lines in_chan) ~f:(Int.of_string)  in
        find_repeat in_list (0, IntSet.empty, false)



let main =
        Printf.printf "return value p1: %d\n" part_one;
        Printf.printf "return value p2: %d\n" part_two
