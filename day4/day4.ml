open Core
open In_channel
open Base

let file = "./input_1.txt"

module GuardTimeStamp = struct
        type status = Start | StartSleep | EndSleep [@@deriving sexp,hash,compare]
        type t = {
                year: int;
                month: int;
                day: int;
                hour: int;
                minute: int;
                id: int option;
                status: status;
        }[@@deriving sexp,hash,compare]
end

let parse_records (in_str: string) : GuardTimeStamp.t option  =
        let sleep_status =
        try let _ = Str.search_forward (Str.regexp "asleep") in_str 0 in GuardTimeStamp.StartSleep
        with _ -> GuardTimeStamp.EndSleep in
        let rgx_id_match = Str.regexp "[^0-9]+" in
        match Str.split rgx_id_match in_str |> List.map ~f:(Int.of_string) with
        | [y; mo; d; h; min; id] -> Some({year = y; month = mo; day = d; hour = h; minute = min; id = Some(id); status = Start})
        | [y; mo; d; h; min] -> Some({year = y; month = mo; day = d; hour = h; minute = min; id = None; status = sleep_status })
        | _ ->  None

let sort_records records =
                (*there's got to be a better way to do this*)
                records
                |> List.stable_sort ~compare:(fun (a: GuardTimeStamp.t) (b: GuardTimeStamp.t) -> Int.compare a.minute b.minute)
                |> List.stable_sort ~compare:(fun (a: GuardTimeStamp.t) (b: GuardTimeStamp.t) -> Int.compare a.hour b.hour)
                |> List.stable_sort ~compare:(fun (a: GuardTimeStamp.t) (b: GuardTimeStamp.t) -> Int.compare a.day b.day)
                |> List.stable_sort ~compare:(fun (a: GuardTimeStamp.t) (b: GuardTimeStamp.t) -> Int.compare a.month b.month)
                |> List.stable_sort ~compare:(fun (a: GuardTimeStamp.t) (b: GuardTimeStamp.t) -> Int.compare a.year b.year)


(*let find_sleepiest_guard records: int =*)
        (*let sorted_records = sort_records records in*)
        (*let parsed_records = parse_*)
        (*let _ = List.foldi sorted_records ~init:(Core.Hashtbl.create)*)
                (*~f:(fun idx acc ele ->*)
                        (*let _ = printf in acc*)


                (*)*)
        (*in 5*)
        (*sort records by min, hour, day, month, year inplace*)
        (*fold over records; acc is (current_gua*)


        (*let applied_rgx = Str.split rgx in_str in*)

let part_one : int =
        let records = create file
        |> input_lines
        |> List.map ~f:(parse_records)
        |> sort_records
        in
        5

let main =
        printf "\nreturn value p1: %d\n" part_one;
