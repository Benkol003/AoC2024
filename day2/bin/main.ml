open Printf

let content =
  let ic = open_in "input" in
  let len = in_channel_length ic in
  let _content = really_input_string ic len in
  close_in ic; _content

let reports =
  let lines = List.filter (fun s -> s<>"") (String.split_on_char '\n' content) in (* filter to get around last newline*)
  List.map (fun line -> let ns = String.split_on_char ' ' line in List.map int_of_string ns) lines;;

let sign : int -> int = fun x ->
  if x > 0 then 1
  else if x < 0 then -1
  else 0


let rec check_report_r : int -> int list -> int -> bool = fun head report dir ->
  match report with
  | [] -> true
  | eh::et -> if sign (eh-head)<>dir || abs (eh-head) > 3 then false else check_report_r eh et dir


let check_report : int list -> bool = fun report ->
  let (e1,e2,el) = match report with
  | [] -> failwith "empty report"
  | e1 :: em -> match em with
                | e2 :: el -> (e1,e2,el)
                | [] -> failwith "report len 1" in

  let dir = sign (e2-e1) in check_report_r e1 (e2::el) dir;;

printf "part 1: %d\n" ( List.fold_left (fun acc i -> if check_report i then acc+1 else acc) 0 reports);;

(*//////////////*)

(* only allow dampening once i.e. remove element in front on a fail*)
(*
unsafe transition 
args a - b - c - [dl]
b-c unsafe
either a - c::dl safe or b-dl safe (fall back to check_report)

dl empty:
  fine with check_report
else if unsafe level then only call check_report _dampened_r 
*)
let rec check_report_dampened_r : int -> int -> int -> int list -> int -> bool = fun a b c dl dir ->
  if sign (c-b)<>dir || abs (c-b) > 3 then
    ( check_report_r a (c::dl) dir ) || (check_report_r b dl dir)
  else
    match dl with
    | [] -> true
    | d::dl -> check_report_dampened_r b c d dl dir

let check_report_dampened : int list -> bool = fun report ->
  match report with
  | [] -> true
  | _::[] -> true
  | a::b::[] ->  abs (b-a) < 3 && sign(b-a)<>0
  | a::b::c::dl -> abs (b-a) < 3 && sign(b-a)<>0 && check_report_dampened_r a b c dl (sign (b-a))

let _ = printf "part 2: %d\n" ( List.fold_left (fun acc i -> if check_report_dampened i then acc+1 else acc) 0 reports);;