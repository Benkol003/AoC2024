open Shared

let%test _ = print_endline "starting 'Shared' tests"; true;;
let%test _ = (count_occurences "SMAS" "SMASMASMASxxSMASsmas")=4
let%test _ = (count_occurences "XMAS" "XMAS ")=1
let%test _ = (array_reverse [|1;2;3;4;5;|])=[|5;4;3;2;1|]
let%test _ = (array_to_string [|'a';'b';'c';'d'|])="abcd"
let%test _ = (list_get ((List.length [1;2;3])/2) [1;2;3] )=2
