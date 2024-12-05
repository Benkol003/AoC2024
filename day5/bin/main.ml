open Shared
open Printf

let input = readlines_file "./day5/input";;

print_newline();;
print_endline "input";;
List.iter (fun i -> print_endline i) input;;

let (rules,pages) = 
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
List.iter (fun i -> print_endline i) pages;;

() ;;

(* assuming each page only occurs once*)

let string_to_rule str = let x = (String.split_on_char '|' str) in match x with
  | [lp;rp] -> (lp,rp)
  | _ -> failwith "failed to parse rule" 
;;

let _ = List.fold_left (fun acc pagestr -> let page = (String.split_on_char ',' pagestr) in 
  if (List.fold_left (fun acc rule -> acc && (
    let (lp,rp) = string_to_rule rule in 
      not (List.exists (fun x -> x=lp) page) || not (List.exists (fun x -> x=rp) page) || (*if pages of the rule dont exist then dont check*)
      (*check rhat left page occurs before right page*)
      ( List.find_index (fun x -> x=lp) page < List.find_index (fun x -> x=rp) page)
      (* if obeys all rules then find middle of list and accumulate that value*)
)) true rules)=true then let midval = int_of_string (list_get ((List.length page)/2) page) in acc + midval else acc) 0 pages |> printf "part 1: %d\n";;