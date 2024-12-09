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

let updates  = Array.of_list updates |> Array.map  (fun str -> String.split_on_char ',' str |> Array.of_list);;
let rules = Array.of_list rules;;

print_newline();;
print_endline "rules";;
let print_string_array sa delim = let _ = Array.iter (fun i -> printf"%s%s" i delim) sa in print_newline ();;

print_string_array rules "\n";;

print_newline();;
print_endline "pages";;
Array.iter (fun i -> Array.fold_left (fun acc x -> acc^","^x) "" i |>print_endline) updates;;

() ;;

(* assuming each page only occurs once*)

let string_to_rule str = let x = (String.split_on_char '|' str) in match x with
  | [lp;rp] -> (lp,rp)
  | _ -> failwith "failed to parse rule" 
;;


let part1 update acc (lp,rp) =  acc && (
      (*check rhat left page occurs before right page*)
      ( Array.find_index (fun x -> x=lp) update < Array.find_index (fun x -> x=rp) update)
      (* if obeys all rules then find middle of list and accumulate that value*)
)

let userule update (lp,rp) =
  (Array.exists (fun x -> x=lp) update) && (Array.exists (fun x -> x=rp) update);;
      

let check_update update = Array.fold_left (fun acc' strrule -> let rule = string_to_rule strrule in if (userule update rule) then (part1 update acc' rule) else acc') true rules

(*if pages of the rule dont exist then dont check*)

let (correct_updates, incorrect_updates) = Array.fold_left (fun (acc_l,acc_r) update -> 
  match check_update update with
  | true -> ((update::acc_l),acc_r)
  | false -> (acc_l,(update::acc_r))
  ) ([],[]) updates;;


let sum_midpoints list = List.fold_left (fun acc update -> let midval = int_of_string update.((Array.length update)/2) in acc + midval) 0 list;;

  (*initial assumption all updates are valid until fail a rule*)
sum_midpoints correct_updates |> printf "part 1: %d\n";;

let _ = incorrect_updates;;

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

but a > c if we do swaps for a < b and b < c then one will be reverted 
screw it just do it in a for loop until it satisfies all ruels (the alternative is sorting the rules / making a graph)

this code has become a mess, forgive me
*)

let swap_fix_pass update = Array.iter (fun strrule -> let (lp,rp) = string_to_rule strrule in if (userule update (lp,rp)) 
  then let lpi = (Array.find_index (fun x -> x=lp) update) |> Option.get in let rpi = (Array.find_index (fun x -> x=rp) update) |> Option.get in
  if not (lpi<rpi) then let _ = (array_swap update lpi rpi) in ()  else () ) rules;;

let rec fix_update update = match (check_update update) with 
  | true -> update
  | false -> let _ = swap_fix_pass update in fix_update update

(*this fixes the incorrect updates in place*)
let _ = List.iter (fun u -> let _ = fix_update u in () ) incorrect_updates;;

(*initial assumption all updates wont be used (false) until fail all rules*)
let _ = sum_midpoints incorrect_updates |> printf "part 2: %d\n";;