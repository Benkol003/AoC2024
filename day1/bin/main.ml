open Printf
open Str

let content =
let ic = open_in "input" in
let len = in_channel_length ic in
let _content = really_input_string ic len in
close_in ic; _content

let (l1, l2) = 
let l1 = ref [] in
let l2 = ref [] in
let parts = String.split_on_char '\n' content in
List.iter (fun line -> 
    let parts = Str.split(regexp "   ") line in
    match parts with
    | [x; y] -> l1 := (int_of_string x) :: !l1;
                l2 := (int_of_string y) :: !l2
    | _ -> (print_string "input error")) parts; (!l1,!l2)

(*
let l1 = [3;4;2;1;3;3;];;
let l2 = [4;3;5;3;9;3];;
*)

let _print_list_int lst =
    print_char '[';
    List.iter (fun i -> printf "%d " i) lst;
    print_char ']';;

let p1 =
    let l1 = List.sort compare l1 in 
    let l2 = List.sort compare l2 in
    let ldiff = List.map2 (fun x y -> abs (x-y) ) l1 l2 in
    List.fold_left (fun acc x -> acc+x) 0 ldiff;;

printf "part 1: %d\n" p1;;

(*
probability of repeated numbers decreases with larger inputs
*)

let p2 = 
    List.fold_left(fun acc1 v1 -> acc1 + v1*(
        List.fold_left(fun acc2 v2 -> if v1=v2 then acc2+1 else acc2 ) 0 l2
    )) 0 l1;;

printf "part 2: %d\n" p2;;
    