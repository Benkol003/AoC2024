open Printf

let content =
  let ic = open_in "input" in
  let len = in_channel_length ic in
  let _content = really_input_string ic len in
  close_in ic; _content

let levels =
  let lines = List.filter (fun s -> s<>"") (String.split_on_char '\n' content) in (* filter to get around last newline*)
  List.map (fun line -> let ns = String.split_on_char ' ' line in List.map int_of_string ns) lines;;


List.iter (fun level -> List.iter (fun l -> printf "%d " l ) level; print_newline () ) levels;;

let sign : int -> int = fun x ->
  if x > 0 then 1
  else if x < 0 then -1
  else 0


let rec check_level_r : int -> int list -> int -> bool = fun head level dir ->
  match level with
  | [] -> true
  | eh::et -> if sign (eh-head)<>dir || abs (eh-head) > 3 then false else check_level_r eh et dir


let check_level : int list -> bool = fun level ->
  let (e1,e2,el) = match level with
  |  [] -> failwith "empty list"
  | e1 :: em -> match em with
                | e2 :: el -> (e1,e2,el)
                | [] -> failwith "list len 1" in

  let dir = sign (e2-e1) in
  if (e2-e1)=0 || abs (e2-e1)>3 then false else check_level_r e2 el dir;;

printf "part 1: %d\n" ( List.fold_left (fun acc i -> if check_level i then acc+1 else acc) 0 levels);;


printf "part 2: %d\n" 0;;
(*
in a level if theres an error, remove one ahead, see if then valid with at i+2 else return false
also can only do this once

two sets of logic for init and recurse states...

*)