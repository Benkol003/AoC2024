open Printf
let print_int_list : int list -> unit = fun l -> List.iter(fun i -> printf "%d " i) l;;
let remove_one_all : int list -> int list list = fun l ->
  let rec aux : int list -> int list -> int list list -> int list list = fun before after acc ->
    match after with
      | [] -> acc
      | (eh::after') -> aux (before@[eh]) after' (acc@[before@after']) in let acc = [] in aux [] l acc
  ;;

let rec list_get i list =
  match list with
  | (h::t) -> if i=0 then h else (list_get (i-1) t)
  | _ -> failwith "index out of range" 
  ;;

let readlines_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let file_as_string fname = 
  let ic = open_in fname in
  let len = in_channel_length ic in
  let _content = really_input_string ic len in
  close_in ic; _content;;

let transpose matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  Array.init cols (fun i ->
    Array.init rows (fun j ->
      matrix.(j).(i)
    )
  )

  let array_to_string a =
    String.of_seq (Array.to_seq a)

let string_to_array s =
  Array.init (String.length s) (fun i -> String.get s i)

let print_char_matrix matrix = 
  Array.map (fun i -> array_to_string i |> print_endline) matrix




let array_reverse a = 
    let ret = (Array.make (Array.length a) a.(0) ) in
    for i = 0 to ((Array.length a)-1) do
      Array.set ret ((Array.length a)-1-i) a.(i)
    done; ret
let count_occurences : string -> string -> int = fun findStr string ->
  let rec aux acc pos =
    if pos > (String.length string) - (String.length findStr) then acc else (
      if (String.sub string pos (String.length findStr))=findStr then aux (acc+1) (pos+1) else aux acc (pos+1)
    ) in aux 0 0;;
  

let array_swap array i j =
    let tmp = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- tmp; array;;