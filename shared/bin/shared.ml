open Printf

let print_int_list : int list -> unit = fun l -> List.iter(fun i -> printf "%d " i) l;;

let remove_one_all : int list -> int list list = fun l ->
  let rec aux : int list -> int list -> int list list -> int list list = fun before after acc ->
    match after with
      | [] -> acc
      | (eh::after') -> aux (before@[eh]) after' (acc@[before@after']) in let acc = [] in aux [] l acc


let file_as_string fname = 
  let ic = open_in fname in
  let len = in_channel_length ic in
  let _content = really_input_string ic len in
  close_in ic; _content;;

  (*
  let content =
    let lines = List.filter (fun s -> s<>"") (String.split_on_char '\n' content_raw) in (* filter to get around last newline*)
    List.map (fun line -> let ns = String.split_on_char ' ' line in List.map int_of_string ns) lines;;
    *)
