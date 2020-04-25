open Core
open In_channel
open Base

let file = "./input_1.txt"

module IntMap = Core.Hashtbl.Make(Int);;

module TimeStamp = struct
        type t = {
                year: int;
                month: int;
                day: int;
                hour: int;
                minute: int;
                id: int option;
        }[@@deriving sexp, hash, show]

        exception MisformattedTimeStamp of string
        let compare (r_1 : t) (r_2 : t) : int =
                let neq = (fun x y -> Int.equal x y |> not) in
                if neq r_1.year r_2.year then (r_1.year - r_2.year)
                else if neq r_1.month r_2.month then (r_1.month - r_2.month)
                else if neq r_1.day r_2.day then (r_1.day - r_2.day)
                else if neq r_1.hour r_2.hour then (r_1.hour - r_2.month)
                else if neq r_1.minute r_2.minute then (r_1.minute - r_2.minute)
                else MisformattedTimeStamp "two duplicate records!" |> raise
end

module TimeStampSet = Core.Set.Make(TimeStamp);;

module GuardRecord = struct
        type status = Start | StartSleep | EndSleep [@@deriving sexp, hash, compare]
        type parsed_record = {
                ts: TimeStamp.t;
                status: status;
                (*TODO why does deriving show result in compilation failing*)
        }[@@deriving sexp, hash, compare]

        exception MisformattedRecord of string

        let first (a, _, _) = a

        let sort_records records : parsed_record list =
                records |> List.sort ~compare:(fun a b -> TimeStamp.compare a.ts b.ts)

        let parse_records (in_str: string) : parsed_record =
                let sleep_status =
                try let _ = Str.search_forward (Str.regexp "asleep") in_str 0 in StartSleep
                with _ -> EndSleep in
                let rgx_id_match = Str.regexp "[^0-9]+" in
                match Str.split rgx_id_match in_str |> List.map ~f:(Int.of_string) with
                | [y; mo; d; h; min; id] -> {ts = {year = y; month = mo; day = d; hour = h; minute = min; id = Some(id);}; status = Start}
                | [y; mo; d; h; min] -> {ts = {year = y; month = mo; day = d; hour = h; minute = min; id = None;}; status = sleep_status}
                | _ ->  MisformattedRecord "did not match pattern" |> raise

        (*(*it is assumed that the records are sorted by timestamp*)*)
        let group_records (records: parsed_record list) : (parsed_record list) list =
                List.fold records ~init:(Core.Stack.create ()) ~f:(fun acc record ->
                        match record.status with
                        | Start -> Core.Stack.push acc [record]; acc
                        | _ -> match Core.Stack.pop acc with
                                | None -> MisformattedRecord "missing record!" |> raise
                                | Some(x) -> List.append x [record] |> Core.Stack.push acc; acc
                ) |> Core.Stack.to_list

        (*should be sleep, then wake up*)
        let get_time_set_asleep r_1 r_2 : TimeStampSet.t =
                (*sanity check on year; the month and day won't necessarily match, but this is fine*)
                let range = match r_1.ts.hour with
                | 0 -> List.range ~start:(`inclusive) ~stop:(`exclusive) r_1.ts.minute r_2.ts.minute
                | _ -> MisformattedRecord "incorrect hour" |> raise
                in
                List.fold range ~init:(TimeStampSet.empty) ~f:(fun acc ele ->
                        (*I feel like this is also super verbose...*)
                        TimeStampSet.singleton {year = r_2.ts.year; month = r_2.ts.month; day = r_2.ts.day; hour = r_2.ts.hour; minute = ele; id = r_2.ts.id}
                        |> TimeStampSet.union acc
                )



        let get_hashtbl (records: parsed_record list list) : TimeStampSet.t IntMap.t =
                List.fold records ~init:(IntMap.create ())
                ~f:(fun acc ele ->
                        List.fold ele ~init:((acc, None, -1)) ~f:(fun (map, prev_record, id) ele ->
                                match prev_record with
                                (*must be first record*)
                                | None -> (match ele.ts.id with
                                        | None -> MisformattedRecord "initial record must contain ID" |> raise
                                        | Some(id_inner) -> (map, Some(ele), id_inner)
                                )
                                | Some(x) -> (match x.status with
                                        | StartSleep -> let _ =
                                                        IntMap.change map id ~f:(fun old_set ->
                                                                let time = get_time_set_asleep x ele in
                                                                match old_set with
                                                                | None -> Some(time)
                                                                | Some(y) -> Some(TimeStampSet.union y time)
                                                        )
                                                        (*IntMap.change map id (*Int.+ x |> IntMap.find id*)*)
                                                        in (map, Some(ele), id)
                                        | Start | EndSleep -> (map, Some(ele), id)
                                )
                        ) |> first
                )

        let create_record_map (file_name : string) : TimeStampSet.t IntMap.t =
                let tmp = create file_name
                |> input_lines
                |> List.map ~f:(parse_records)
                |> sort_records
                in let _ = List.map tmp ~f:(fun ele ->  sexp_of_parsed_record ele |> Sexp.to_string |> printf "\n %s\n")
                in tmp
                |> group_records
                |> get_hashtbl

        let get_max_and_freq tbl ~get_len =
                Hashtbl.fold tbl ~init:((-1, -1))
                        ~f:(fun ~key:key ~data:data (max_id, max_len) ->
                                (*let len = Core.Set.length data in*)
                                let len = get_len data in
                                if len > max_len then (key, len)
                                else (max_id, max_len)
                        )


        (*key and max value returned as tuple*)
        let get_max_over_set tbl ~get_len : int =
                get_max_and_freq tbl ~get_len:(get_len)
                |> fst

        (*from set of minutes to a hashtable of minute to frequency*)
        let get_minute_freq (entries: TimeStampSet.t) =
                entries
                (*fold the set into a hashtable of minute to number of occurrences*)
                |> TimeStampSet.fold ~init:(IntMap.create ())
                        ~f:(fun acc ele ->
                                IntMap.change acc ele.minute
                                ~f:(fun orig_val ->
                                        match orig_val with
                                        | None -> Some(1)
                                        | Some(x) -> Some(x+1)
                                );
                                acc
                        )

        (*from set of minutes to integer of most frequent minute*)
        let get_most_often_minute (entries: TimeStampSet.t) =
                let freqs = entries |> get_minute_freq in (*take the max over this hashtable*)
                get_max_over_set freqs ~get_len:(fun x -> x)


end

let part_one : int =
        let record_map = GuardRecord.create_record_map file in
        let sleepiest_guard_id = GuardRecord.get_max_over_set record_map ~get_len:(TimeStampSet.length) in
        let sleepiest_guard_most_often_min = IntMap.find_exn record_map sleepiest_guard_id
        |> GuardRecord.get_most_often_minute
        in sleepiest_guard_id * sleepiest_guard_most_often_min

let part_two : int =
        let record_map = GuardRecord.create_record_map file in
        let most_frequent_minute_per_guard = IntMap.map record_map
                (*returns tuple (minute, freq)*)
                ~f:(fun set ->
                        GuardRecord.get_minute_freq set |> GuardRecord.get_max_and_freq ~get_len:(fun x -> x)
                )
        in
        let (max_id, max_min, _) = IntMap.fold most_frequent_minute_per_guard ~init:((-1, -1, -1))
                ~f:(fun ~key:id ~data:(min, freq) (max_id, max_min, max_freq)->
                        if freq > max_freq then (id, min, freq)
                        else (max_id, max_min, max_freq)
                )
        in
        max_id*max_min

let main =
        printf "\nreturn value p1: %d\n" part_one;
        printf "\nreturn value p2: %d\n" part_two;
