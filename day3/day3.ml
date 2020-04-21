open Core
open In_channel
open Base

let file = "./input_1.txt"

module IntTuple = struct
        type t = (int * int) [@@deriving sexp]
        let compare = (fun (a, b) (c, d) ->
                (a + b) - (c + d)
        )
end

module IntTupleSet = Core.Set.Make(IntTuple);;


let gen_tuples width_offset height_offset width height: IntTupleSet.t =
        IntTupleSet.singleton (5, 5)

let parse_params (in_str: string) : IntTupleSet.t =
        (*TODO use regex here*)
        IntTupleSet.singleton (5, 5)

let part_one : int =
        5

let main =
        printf "return value p1: %d\n" part_one;
