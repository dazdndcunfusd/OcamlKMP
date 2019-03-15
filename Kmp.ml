type result = (* types help handling whether success or failure *)
  | Found of int       (* the offset k where a match lies *)
  | Interrupted of int (* the state j that was reached *)

let assertInterrupted = function
  | Found _       -> assert false
  | Interrupted j -> j

let print_result result =
        match result with (*TODO : add extra dialogue for success and failure *)
        | Found x -> print_string "found result at character ";print_int x ; print_newline ()
        | Interrupted x -> print_string "Error at line ";print_int x ; print_newline ()

(* Brute force Implementation -- NOT KMP *)
let rec search pattern pattern_len text text_len j k =
        if j = pattern_len then
                Found (k - j)
        else if k = text_len then
                Interrupted j
        else if pattern.[j] = text.[k] then
                search pattern pattern_len text text_len (j + 1) (k + 1)
        else
                search pattern pattern_len text text_len 0 (k - j + 1)
        (* lots of backtracking. O(m*(n-1)) AKA terrible *)



let prelim_check pattern pattern_len text text_len = 
        if (pattern_len > text_len) || (pattern_len <1) || (text_len < 1) then
                Interrupted 0
        else
                search pattern pattern_len text text_len 0 0

let check_if_file_or_text s1 s2 =
        () (*TODO: add functionality for text files as well. returns type File if file. *)


let () = 
        let args_n = Array.length Sys.argv - 1 in
        let args_list = Array.to_list (Array.sub Sys.argv 1 args_n) in
        let pattern = List.nth args_list 0 in
        let text = List.nth args_list 1 in
        let pattern_len = String.length pattern in
        let text_len = String.length text in
        (* currently only accepts pure string arguments, not text files *)
        let result = prelim_check pattern pattern_len text text_len in
        print_result result
