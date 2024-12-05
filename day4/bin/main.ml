(*
up down left right and diagonal (2 dirs)
and reversed for each of those

do left right scan

up /down rotate the array

diagonal - traverse manually and only for diags > 4

*)
open Shared
open Printf
open Pcre

let string_to_array s =
  Array.init (String.length s) (fun i -> String.get s i)

let print_char_matrix matrix = 
    Array.map (fun i -> array_to_string i |> print_endline) matrix

let input = let i = file_as_string "input" in let il = String.split_on_char '\n' i in let ila = (List.map (fun j -> string_to_array j) il) in Array.of_list ila;;

let transpose matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  Array.init cols (fun i ->
    Array.init rows (fun j ->
      matrix.(j).(i)
    )
  );;

(* sanity check *)
print_endline (if (input=(transpose (transpose input))) then "transpose test : PASSED" else "tranpose test: FAILED");;

let diagonals_right matrix = 
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let ret = ref [] in
  for r = 0 to rows-1 do (*iterate over per-row diagonals*)
    let diag = ref [] in
    (* move diagonally from start of diagonal*)
    for c = 0 to min (cols-1) (rows-1-r) do
      diag := (matrix.(r+c).(c) :: !diag) (* diag being reverse order is fine as we'll search both ways*)
    done;
    ret := (diag :: !ret)
  done;
  for c = 1 to cols-1 do (* iterate over column diagonals (except 0,0 as in row diagonals)*)
    let diag = ref [] in
    for r = 0 to min (cols-1-c) (rows-1) do
      diag := (matrix.(r).(r+c) :: !diag) (* diag being reverse order is fine as we'll search both ways*)
    done;
    ret := (diag :: !ret)
  done; 
  List.map (fun i -> (Array.of_list !i)) !ret |> Array.of_list ;;

print_endline (if (  let rows = Array.length input in let cols = Array.length input.(0) in (rows+cols-1)=Array.length (diagonals_right input) ) then "diagonals_right test : PASSED" else "diagonals_right test: FAILED");;
let diags_r = (diagonals_right input)
let diags_l = diagonals_right (transpose input)

let xmas_rgx = regexp "XMAS";;

(* checking using regex only per-row *)
let xmas_count_row matrix = Array.fold_left (fun acc arr -> ( acc+Array.fold_left (fun acc2 count -> acc2+count) 0 (printf "tt %s" (array_to_string arr); pcre_exec ~rex: xmas_rgx (array_to_string arr)))) 0 matrix;;
(* regex doesnt behave as expected -_-...*)
let test = [|string_to_array "XMAS"|];; 
print_endline "test diags: ";;
diagonals_right test |> print_char_matrix;;
xmas_count_row test  |> printf "test count %d";;

 (xmas_count_row input) + (transpose input |> xmas_count_row ) + (xmas_count_row diags_r) + (xmas_count_row diags_l) |> printf "total %d\n";;