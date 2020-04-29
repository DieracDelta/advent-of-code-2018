open Core
open In_channel
open Base

module IntSet = Core.Set.Make(Int);;

module IntTuple = struct
  type t = (int * int) [@@deriving sexp, compare, hash]
end
module IntTupleSet = Core.Set.Make(IntTuple);;
module IntTupleMap = Core.Map.Make(IntTuple);;

exception IncorrectlyParsed of string

let print_point point =
  printf "(%d, %d) " (fst point) (snd point)

let print_set set =
  IntTupleSet.iter set ~f:(fun ele -> print_point ele)

let split_to_pair (input: string) : IntTupleSet.t =
  let rgx = Str.regexp "[^0-9]+" in
  match Str.split rgx input |> List.map ~f:(Int.of_string) with
  | [x; y] -> IntTupleSet.singleton (x, y)
  | _x -> IncorrectlyParsed "too many coordinates" |> raise

let manhattan_dist (x_1, y_1) (x_2, y_2)=
  (Int.abs (x_1 - x_2)) + (Int.abs (y_1 - y_2))


(*parses out the set of points of interest from input list*)
let get_poi l =
  List.fold l ~init:(IntTupleSet.empty) ~f:(
    fun acc ele_str ->
      split_to_pair ele_str |> IntTupleSet.union acc
  )

let get_extreme_value l ~op ~choose =
  let cmp = fun x y -> if op (choose x) (choose y) then Some(x) else Some(y) in
  let r_val = IntTupleSet.fold l ~init:(None) ~f:(fun acc ele ->
      match acc with
      | None -> Some(ele)
      | Some(max) -> cmp max ele
    ) in
  match r_val with
  | None -> IncorrectlyParsed "Empty range" |> raise
  | Some(x) -> x |> choose

let gen_bb_vals l =
  let left = get_extreme_value l ~op:Int.(<) ~choose: fst in
  let right = get_extreme_value l ~op:Int.(>) ~choose:fst in
  let top = get_extreme_value l ~op:Int.(>) ~choose:snd in
  let bottom = get_extreme_value l ~op:Int.(<) ~choose:snd in
  (left, right, top, bottom)

let gen_bb extreme_vals =
  let (left, right, top, bottom) = extreme_vals in
  let gen_range = List.range ~start:`inclusive ~stop:`inclusive in
  List.cartesian_product (gen_range left right) (gen_range bottom top) |> IntTupleSet.of_list

let map_bb_to_elements psoi bb =
  (*iterate through all elements in the bounding box*)
  (*construct hashmap from owner point tuple to owned point set*)
  IntTupleSet.fold bb ~init:(IntTupleMap.empty) ~f:(fun r_tbl point ->
      (*fold over points of interest to construct poi -> value mapping*)
      let poi_dist_map = IntTupleSet.fold psoi ~init:(IntTupleMap.empty) ~f:(
          fun acc poi -> let dist = manhattan_dist poi point in IntTupleMap.add_exn acc ~key:poi ~data:dist
        ) in
      (*fold over mapping to get smallest distance*)
      (*returns smallest distance, set of points that satisfy that*)
      let (_min_dist, owners) = IntTupleMap.fold poi_dist_map ~init:((None, IntTupleSet.empty)) ~f:(
          fun ~key:point ~data:dist (min_dist_wrapper, min_set) ->
            match min_dist_wrapper with
            | None -> (Some(dist), IntTupleSet.singleton point)
            | Some(min_dist) ->
              if Int.equal dist min_dist then (min_dist_wrapper, IntTupleSet.singleton point |> IntTupleSet.union min_set )
              else if dist < min_dist then (Some(dist), IntTupleSet.singleton point)
              else (min_dist_wrapper, min_set)
        )
      (*if that distance is a tie, do nothing, else add to table*)
      in match IntTupleSet.length owners with
      | 0 -> IncorrectlyParsed "no owner" |> raise
      | 1 ->
        IntTupleMap.change r_tbl (IntTupleSet.choose_exn owners) ~f:(
          fun cur_set ->
            match cur_set with
            | None -> Some(IntTupleSet.singleton point)
            | Some(x) -> Some(IntTupleSet.union x (IntTupleSet.singleton point))
        )
      | _ -> r_tbl
    )

let less_than psoi total bb =
  (*iterate through all elements in the bounding box*)
  (*construct hashmap from owner point tuple to owned point set*)
  IntTupleSet.fold bb ~init:(0) ~f:(fun acc point ->
      (*fold over points of interest to construct poi -> value mapping*)
      let point_val = IntTupleSet.fold psoi ~init:(0) ~f:(
          fun net_sum poi -> let dist = manhattan_dist poi point in net_sum + dist
        )
      in if point_val < total then acc + 1 else acc
    )


let is_infinite extremes point =
  let (l, r, t, b) = extremes in
  let (x, y) = point in
  (Int.equal x l || Int.equal x r || Int.equal y t || Int.equal y b) |> not


let file = "./input_1.txt"
let part_one : int =
  let poi = create file |> Stdio.In_channel.input_lines |> get_poi in
  let extreme_vals = gen_bb_vals poi in
  let tbl = gen_bb extreme_vals |> map_bb_to_elements poi in
  let tbl = IntTupleMap.filter_keys tbl ~f:(is_infinite extreme_vals) in
  let max = IntTupleMap.fold tbl ~init:(None) ~f:(fun ~key ~data acc ->
      let len = IntTupleSet.length data in
      match acc with
      | None -> Some(key, len)
      | Some((max_point, max_len)) -> if len > max_len then Some(key, len) else Some(max_point, max_len)
    )
  in match max with
  | Some(_point, len) -> print_point _point; len
  | None -> IncorrectlyParsed "empty hashtbl" |> raise

let part_two : int =
  let total = 10000 in
  let poi = create file |> Stdio.In_channel.input_lines |> get_poi in
  let extreme_vals = gen_bb_vals poi in
  gen_bb extreme_vals |> less_than poi total

let main =
  printf "\nreturn value p1: %d\n" part_one;
  printf "\nreturn value p2: %d\n" part_two;
