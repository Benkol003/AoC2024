open Shared
open Printf

let input = readlines_file "./day5/input";;

print_newline();;
print_endline "input";;
List.iter (fun i -> print_endline i) input;;

let (rules,updates) = 
  let rec aux r p =
    match p with
    | [] -> failwith "EOF before recieved pages itinary"
    | (head::tail) -> if head="" then (r,tail) else (aux (head::r) tail) in
  aux [] input;;

print_newline();;
print_endline "rules";;
List.iter (fun i -> print_endline i) rules;;

print_newline();;
print_endline "pages";;
List.iter (fun i -> print_endline i) updates;;

() ;;

(* assuming each page only occurs once*)

let string_to_rule str = let x = (String.split_on_char '|' str) in match x with
  | [lp;rp] -> (lp,rp)
  | _ -> failwith "failed to parse rule" 
;;


let part1 update acc (lp,rp) =  acc && (
      (*check rhat left page occurs before right page*)
      ( List.find_index (fun x -> x=lp) update < List.find_index (fun x -> x=rp) update)
      (* if obeys all rules then find middle of list and accumulate that value*)
)

(*if pages of the rule dont exist then dont check*)
let userule update (lp,rp) =
  not (List.exists (fun x -> x=lp) update) || not (List.exists (fun x -> x=rp) update);;

let part_x update_checker = List.fold_left (fun acc strupdate -> let update = (String.split_on_char ',' strupdate) in 
  if (List.fold_left (fun acc strrule -> let rule = string_to_rule strrule in (userule update rule) || (update_checker update acc rule)) true rules)=true then let midval = int_of_string (list_get ((List.length update)/2) update) in acc + midval else acc) 0 updates;;

part_x part1 |> printf "part 1: %d\n";;

(* part 2*)

(*
rules

can we swap positions of two pages without breaking other rules?

a < b
b < c
a < c
ordering a < b < c that is valid
doing a swap e.g. if b < a then swap to a < b
b < c is still satisfied
the a < b < c ordering should hold

right now we need everything as arrays -_-
*)

let part2 update acc (lp,rp) = acc && (
  if (List.find_index (fun x -> x=lp) update < List.find_index (fun x -> x=rp) update) then true else (
    false
  )
)
