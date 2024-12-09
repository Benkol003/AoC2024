open Shared
open Printf

let board = let content = readlines_file "./day6/input" in Array.map (fun line -> Array.of_seq (String.to_seq line)) (Array.of_list content) ;;

let find_start board = 
  let rec aux board y = let x' = Array.find_index (fun v -> v='^') board.(y) in match x' with | Some(x) -> (x,y) | None -> (aux board (y+1)) in
  aux board 0;;

let start_pos = find_start board;;

let (x,y) = start_pos in printf "start: %d,%d\n" x y;;

let rotate_right dir = match dir with
  | (1,0) -> (0,1)
  | (0,1) -> (-1,0)
  | (-1,0) -> (0,-1)
  | (0,-1) -> (1,0)
  | _ -> failwith "invalid direction provided.";;


  (* modifies in-place*)
let rec walk board (posx,posy) (dirx,diry) = 
  board.(posy).(posx)<-'X';
  if (posy+diry) >= (Array.length board) || (posy+diry) <0 || (posx+dirx) >= (Array.length board.(0)) || (posx+dirx)<0 then let _ = printf "finish: %d,%d\n" posx posy in ()
  else (
  if (board.(posy+diry).(posx+dirx)='#') then walk board (posx,posy) (rotate_right (dirx,diry)) else (
    walk board (posx+dirx,posy+diry) (dirx,diry)
  )
  );;

walk board start_pos (0,-1);;

print_endline "walked board:";;
print_char_matrix board;;

(*todo set start pos to x*)


Array.fold_left (fun acc row -> Array.fold_left (fun acc' v -> if v='X' then acc'+1 else acc') acc row) 0 board |> printf "part 1: %d\n";;