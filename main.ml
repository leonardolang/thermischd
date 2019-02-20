open Base
open Stdio
open Printf

(* TODO *********************************************************
    * add "state" structure with:
        - trippoints
        - current point
        - accumulator
    * add support for writing on files as actions? (ie sysfs?)
    * read settings from file, supporting other methods for
      retrieving temperature (like running a program)
 ***************************************************************)

type action =
    Spawns of string list
;;

let hwmon_base = "/sys/class/hwmon";;

let sources =
[
    "hwmon2/temp1";
    "hwmon2/temp2";
    "hwmon2/temp3";
    "hwmon2/temp4";
    "hwmon2/temp5";
    "hwmon2/temp6";
    "hwmon2/temp7"
]
;;

let trippoints =
[|
    ( 0,  65, Spawns [ "/usr/bin/i8kctl"; "fan"; "-"; "0" ]);
    (60,  80, Spawns [ "/usr/bin/i8kctl"; "fan"; "-"; "1" ]);
    (70, 999, Spawns [ "/usr/bin/i8kctl"; "fan"; "-"; "2" ]);
|]
;;

let point_getmin p =
    let (r,_,_) = p in r
;;
let point_getmax p =
    let (_,r,_) = p in r
;;
let point_getaction p =
    let (_,_,r) = p in r
;;

let dbgmsg s =
    let str = try Unix.getenv "DEBUG" with Caml.Not_found -> "0" in
    let num = try Int.of_string str with Failure _ -> 0 in
    if Int.compare num 0 > 0 then print_endline s
;;

let logmsg = print_endline
;;

let get_max_temp lst =
    dbgmsg (sprintf "Reading temperatures...");
    List.fold lst ~init:(0, "none") ~f:begin fun res name ->
        In_channel.with_file (sprintf "%s/%s_input" hwmon_base name)
            ~f:begin fun fd ->
                match In_channel.input_line fd with
                  Some strvalue ->
                    let value = Int.of_string strvalue in
                    let value = if value > 999 then value / 1000 else value in
                    if Int.compare (fst res) value < 0
                    then (value, name)
                    else res

                | None -> res
            end
        end
;;

let check_trippoint data temp =
    (Int.compare temp (point_getmin data)) >= 0 &&
    (Int.compare temp (point_getmax data)) <  0
;;

let find_trippoint temp points cur =
    let valid =
        if Int.compare (-1) cur < 0
        then check_trippoint points.(cur) temp
        else false
    in
        if valid
        then begin
            dbgmsg (sprintf "Temperature %d still matches trippoint %d..." temp cur);
            cur
        end else
            Array.foldi points ~init:cur ~f:begin fun num res data ->
                dbgmsg (sprintf "Checking trippoint %d with temp %d..." num temp);
                if check_trippoint data temp then num else res
            end
;;

let spawn_process prog argv =
    logmsg (sprintf "+ Spawning: %s" (String.concat ~sep:" " argv));
    let pid, inp =
        let nil = Unix.openfile "/dev/null" [ O_RDONLY ] 0o000 in
        let rd, wr = Unix.pipe () in
        let pid = Unix.create_process prog (Array.of_list argv) nil wr wr in
        begin
            Unix.close nil;
            Unix.close wr;
            pid, (Unix.in_channel_of_descr rd)
        end
    in begin
        Stdio.In_channel.iter_lines inp (fun s -> logmsg (sprintf "[%s] %s" prog s));
        let (_,status) = Unix.waitpid [] pid in
        match status with
          WEXITED ret when ret <> 0 ->
            logmsg (sprintf "ERROR: process %s[%d] returned %d" prog pid ret)
        | WSIGNALED s | WSTOPPED s ->
            logmsg (sprintf "ERROR: process %s[%d] killed/stopped by signal %d" prog pid s)
        | WEXITED _ -> ()
    end

let trigger_action points num =
    match point_getaction trippoints.(num) with
      Spawns cmd -> spawn_process (List.nth_exn cmd 0) cmd
;;

let verify_accumulator acc want_acc points got cur =
    if (Int.compare acc want_acc) >= 0 || (Int.compare cur (-1)) = 0
    then begin
        logmsg (sprintf "Trip point changed: %d -> %d (%d cycles), triggering actions..." cur got acc);
        trigger_action points got;
        0, got
    end else begin
        logmsg (sprintf "Trip point %d detected for %d cycles (current is %d)..." got acc cur);
        (acc+1), cur
    end
;;

let main () =
    let rec loops cur acc =
        dbgmsg (sprintf "Running main loop...");
        Out_channel.flush stdout;
        let maxtemp, maxname = get_max_temp sources in
        let got = find_trippoint maxtemp trippoints cur in
        let (acc, got), delay =
            match Int.compare got cur with
              -1 -> (verify_accumulator acc 3 trippoints got cur), 1
            |  1 -> (verify_accumulator acc 2 trippoints got cur), 1
            |  _ -> let acc = if (Int.compare acc 0) > 1 then acc-1 else 1 in (acc, cur), 3
        in begin
            Caml.Gc.full_major ();
            Unix.sleep delay;
            loops got acc
        end
    in
        loops (-1) 1
;;

main ()
;;
