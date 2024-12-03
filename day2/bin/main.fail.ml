(*
this could be algorithmically more efficient if it worked
probably missed some edge cases
*)

open Printf

let content =
  let ic = open_in "input" in
  let len = in_channel_length ic in
  let _content = really_input_string ic len in
  close_in ic; _content

let print_int_list : int list -> unit = fun l -> List.iter(fun i -> printf "%d " i) l

let reports =
  let lines = List.filter (fun s -> s<>"") (String.split_on_char '\n' content) in (* filter to get around last newline*)
  List.map (fun line -> let ns = String.split_on_char ' ' line in List.map int_of_string ns) lines;;

let sign : int -> int = fun x ->
  if x > 0 then 1
  else if x < 0 then -1
  else 0


let rec check_report_r : int -> int list -> int -> bool = fun head report dir ->
  if dir=0 || abs dir > 3 then false else
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
let rec check_report_dampen_r : int -> int -> int -> int list -> int -> bool = fun a b c dl dir ->
  if dir=0 || abs dir > 3 then false else
  if sign (c-b)<>dir || abs (c-b) > 3 then
    ( check_report_r a (c::dl) dir ) || (check_report_r b dl dir)
  else
    match dl with
    | [] -> true
    | d::dll -> check_report_dampen_r b c d dll dir
    
let check_report_dampen : int list -> bool = fun report ->
  match report with
  | [] -> failwith "invalid input"
  | _::[] -> failwith "invalid input"
  | _::_::[] -> failwith "invalid input"
  (* also these edge cases arent in the input anyway*)

  | a::b::c::dl -> match abs (b-a) <= 3 && sign (b-a)<>0 with
                    | true -> 
                      (match abs (c-b) <= 3 && sign(c-b)=sign(b-a) with
                      | true -> check_report_dampen_r a b c dl ( sign(b-a) )
                      | false -> check_report_r a (c::dl) (sign (c-a)) || check_report_r a (b::dl) (sign (b-a)) (* special case, if a-b valid but b-c isnt, and removing b fixes it the new dir is a-c*) 
                      )
                    
                    | false -> ( check_report_r b (c::dl) (sign (c-b)) ) || ( check_report_r a (c::dl) (sign (c-a)) )(* remove either a or b if first transition bad *)

let _ = printf "part 2: %d\n" ( List.fold_left (fun acc i -> (print_int_list i; if check_report_dampen i then (print_endline "safe";acc+1) else (print_endline "unsafe";acc))) 0 reports);;