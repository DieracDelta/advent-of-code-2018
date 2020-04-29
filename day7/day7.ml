open Core
open In_channel
open Base

module CharSet = Core.Set.Make(Char);;
module CharMap = Core.Map.Make(Char);;
exception IncorrectlyParsed of string

module Node = struct
  type t = {
    to_set: CharSet.t;
    from_set: CharSet.t;
  } [@@deriving sexp, compare]
end

(*converts a list of pairs of characters into a map from characters to nodes*)
let convert_to_graph l =
  l |> List.fold ~init:(CharMap.empty) ~f:( fun acc (from_id, to_id) ->
      let result = CharMap.change acc from_id ~f:(fun node_opt ->
          match node_opt with
          | None -> Some({to_set = CharSet.singleton to_id; from_set = CharSet.empty})
          | Some(n) -> let new_to_set = CharSet.singleton to_id |> CharSet.union n.to_set
            in Some({to_set: new_to_set; from_set: n.from_set})

        ) in
      result


    )

let parse_input s =
  (String.get s 5, String.get s 36)


let file = "./input_1.txt"
let part_one : int =
  let _ = create file |> Stdio.In_channel.input_lines |> List.map ~f:parse_input |> convert_to_graph
  in 5


(*let part_two : int =*)
(*let total = 10000 in*)
(*let poi = create file |> Stdio.In_channel.input_lines |> get_poi in*)
(*let extreme_vals = gen_bb_vals poi in*)
(*gen_bb extreme_vals |> less_than poi total*)

let main =
  printf "\nreturn value p1: %d\n" part_one;
  (*printf "\nreturn value p2: %d\n" part_two;*)
