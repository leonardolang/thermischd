open Printf
open Base
open Stdio

(* TODO *********************************************************
    * add support for writing on files as actions? (ie sysfs?)
    * read settings from file, supporting other methods for
      retrieving temperature (like running a program)
 ***************************************************************)

type action =
    Spawns of string * string list
;;

type state =
{
    s_tps: (int * int * action) Array.t;
    s_cur_tp: int;
    s_new_tp: int;
    s_accum: int;
}
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
    ( 0,  65, Spawns ("/usr/bin/i8kctl", [ "fan"; "-"; "0" ]));
    (60,  80, Spawns ("/usr/bin/i8kctl", [ "fan"; "-"; "1" ]));
    (70, 999, Spawns ("/usr/bin/i8kctl", [ "fan"; "-"; "2" ]));
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

let debug_level =
    let strval =
        try Unix.getenv "DEBUG" with Caml.Not_found -> "0"
    in
        try Int.of_string strval with Failure _ -> 0

let dbgmsg lvl s =
    if Int.compare debug_level lvl >= 0 then
    begin
        printf "%s %s\n" (String.make lvl '+') s;
        Out_channel.flush stdout
    end
;;

let logmsg = print_endline
;;

let get_max_temp lst =
    dbgmsg 2 (sprintf "Reading temperatures...");
    List.fold lst ~init:(0, "none") ~f:begin fun res name ->
        let fname = sprintf "%s/%s_input" hwmon_base name in
        In_channel.with_file fname ~f:begin fun fd ->
            match In_channel.input_line fd with
              Some strvalue ->
                dbgmsg 3 (sprintf "Read %s from %s" strvalue fname);
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
            dbgmsg 1 (sprintf "Temperature %d still matches trippoint %d..." temp cur);
            cur
        end else
            Array.foldi points ~init:cur ~f:begin fun num res data ->
                dbgmsg 2 (sprintf "Checking trippoint %d with temp %d..." num temp);
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
      Spawns (cmd, args) -> spawn_process cmd (cmd :: args)
;;

let verify_new_state state maxtemp new_tp want_acc =
    if (Int.compare state.s_cur_tp (-1)) = 0
    then begin
        logmsg (sprintf "Setting initial trip point %d and triggering actions..." new_tp);
        trigger_action state.s_tps new_tp;
        (0, new_tp, new_tp)
    end else
        if (Int.compare state.s_new_tp new_tp) <> 0
        then begin
            dbgmsg 1 (sprintf "New trip point %d detected (temp %dC)..." new_tp maxtemp);
            1, state.s_cur_tp, new_tp
        end else
            if (Int.compare state.s_accum want_acc) >= 0
            then begin
                logmsg (sprintf "Trip point changed: %d -> %d: triggering actions..." state.s_cur_tp new_tp);
                trigger_action state.s_tps new_tp;
                0, new_tp, new_tp
            end else begin
                (state.s_accum + 1), state.s_cur_tp, new_tp
            end
;;

let verify_cur_state state maxtemp =
    if (Int.compare state.s_accum 0) > 1
    then begin
        if (Int.compare state.s_new_tp state.s_cur_tp) <> 0
        then logmsg (sprintf "Trip point %d not detected anymore (temp %dC)..." state.s_new_tp maxtemp);
        (state.s_accum - 1), state.s_cur_tp, state.s_new_tp
    end else
        0, state.s_cur_tp, state.s_cur_tp

let main tps =
    let rec loops state =
        dbgmsg 2 (sprintf "Running main loop...");
        let maxtemp, maxname = get_max_temp sources in
        let new_tp = find_trippoint maxtemp state.s_tps state.s_cur_tp in
        let (accum, cur_tp, new_tp), delay =
            match Int.compare new_tp state.s_cur_tp with
              -1 -> (verify_new_state state maxtemp new_tp 3), 1
            |  1 -> (verify_new_state state maxtemp new_tp 2), 1
            |  _ -> (verify_cur_state state maxtemp), 3
        in begin
            Caml.Gc.major ();
            Unix.sleep delay;
            loops { state with s_cur_tp = cur_tp; s_new_tp = new_tp; s_accum = accum; }
        end
    in
        loops { s_tps = tps; s_cur_tp = (-1); s_new_tp = (-1); s_accum = 1; }
;;

main trippoints
;;
