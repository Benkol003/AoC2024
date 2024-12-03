open Printf

let content =
  let ic = open_in "input" in
  let len = in_channel_length ic in
  let _content = really_input_string ic len in
  close_in ic; _content

let _print_int_list : int list -> unit = fun l -> List.iter(fun i -> printf "%d " i) l;;

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

  let dir = sign (e2-e1) in check_report_r e1 (e2::el) dir

let _ = printf "part 1: %d\n" ( List.fold_left (fun acc i -> if check_report i then acc+1 else acc) 0 reports)

let remove_one_all : int list -> int list list = fun l ->
  let rec aux : int list -> int list -> int list list -> int list list = fun before after acc ->
    match after with
      | [] -> acc
      | (eh::after') -> aux (before@[eh]) after' (acc@[before@after']) in let acc = [] in aux [] l acc
  
let check_report_dampen : int list -> bool = fun report ->
  match check_report report with 
    | true -> true
    | false -> List.exists (fun report_variant -> check_report report_variant) (remove_one_all report)


let _ = printf "part 2: %d\n" ( List.fold_left (fun acc i -> (if check_report_dampen i then acc+1 else acc)) 0 reports)