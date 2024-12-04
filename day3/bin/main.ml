open Printf
open Shared

(**)
let mul_matcher= "mul\\(\\d+,\\d+\\)"
let mul_enable = "do\\(\\)"
let mul_disable= "don't\\(\\)"

let expr = file_as_string "input";;
let mults = Pcre.extract_all ~rex: (Pcre.regexp mul_matcher) expr;;


Array.iter (fun i -> printf "%s \n" (Array.get i 0) ) mults;;

let mul : string -> int = fun expr ->
    let vals = Pcre.extract_all ~rex: (Pcre.regexp "\\d+") expr in
    match vals with 
    | [|x;y|] -> (int_of_string (Array.get x 0)) * (int_of_string (Array.get y 0))
    | err -> failwith (sprintf ("mul - wrong arg count: %d") (Array.length err))
;;


printf "part 1: %d \n" ( Array.fold_left (fun acc i -> acc + mul (Array.get i 0) ) 0 mults );;
(*
  let _ = Array.fold_left (fun acc i -> acc + (mul 1) ) expr 0;;
*)  

let _print_string_array_array arr =
    Array.iter (fun sub_array ->
      Array.iter (fun str -> print_endline str) sub_array
    ) arr
  

let expr = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";;
let expr= "do()" ^ expr;;
print_endline expr
let mult_enabled_blocks = Pcre.extract_all ~rex: (Pcre.regexp (sprintf "(%s.*?(?=%s).*?(?=%s)*)" mul_enable mul_matcher mul_disable)) expr;;

Array.iter()