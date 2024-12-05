(*
up down left right and diagonal (2 dirs)
and reversed for each of those

do left right scan

up /down rotate the array

diagonal - traverse manually and only for diags > 4

*)
open Shared
open Printf

let input = let i = file_as_string "./day4/input" in let il = String.split_on_char '\n' i in let ila = (List.map (fun j -> string_to_array j) il) in Array.of_list ila;;

let transpose matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  Array.init cols (fun i ->
    Array.init rows (fun j ->
      matrix.(j).(i)
    )
  );;

let mirror matrix = Array.map(fun i -> (array_reverse i)) matrix;;

(* sanity check *)
print_endline (if (input=(transpose (transpose input))) then "transpose test : PASSED" else "tranpose test: FAILED");;
print_endline (if (input=(mirror (mirror input))) then "mirror test : PASSED" else "mirror test: FAILED");;

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
let diags_l = input |> mirror |> diagonals_right

(* checking using regex only per-row *)
let xmas_count_row matrix = Array.fold_left (fun acc row -> acc+( let row_str = array_to_string row in (count_occurences "XMAS" row_str)+(count_occurences "SAMX" row_str)) ) 0 matrix;;
(xmas_count_row input) +
(input |> transpose  |> xmas_count_row ) +
(xmas_count_row diags_r) +
(xmas_count_row diags_l) 
|> printf "part 1 total: %d";;


(* part 2
this is just a image kernel
*)

let xmas_kernel_check matrix offset_row offset_column = 
    let tmp = ref [] in let diag_r = for i =0 to 2 do tmp := (matrix.(i+offset_row).(i+offset_column)::!tmp) done; String.of_seq (List.to_seq !tmp)  in
    let tmp = ref [] in let diag_l = for i =0 to 2 do tmp := (matrix.(2-i+offset_row).(i+offset_column)::!tmp) done; String.of_seq (List.to_seq !tmp) in
    if (diag_r="MAS" || diag_r="SAM") && (diag_l="MAS"||diag_l="SAM") then true else false;;


let acc = ref 0 in
for r = 0 to (Array.length input)-3 do
  for c = 0 to (Array.length input.(0))-3 do
    if (xmas_kernel_check input r c)=true then acc:= !acc + 1
  done
done;
printf "part 2: %d" !acc;;