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
let rec search pattern lpattern text ltext j k =
        if j = lpattern then
                Found (k - j)
        else if k = ltext then
                Interrupted j
        else if pattern.[j] = text.[k] then
                search pattern lpattern text ltext (j + 1) (k + 1)
        else if j = 0 then
                search pattern lpattern text ltext 0 (k+1)
        else
                let l = assertInterrupted (search pattern lpattern pattern j 0 1) in
                assert (l<j);
                search pattern lpattern text ltext l k

        (* lots of backtracking. O(m*(n-1)) AKA terrible *)
        (* l, j, and k are all iterators, keeping track of the last known position. *)


(* checks if pattern length is valid. *)
let prelim_check pattern pattern_len text text_len = 
        if (pattern_len > text_len) || (pattern_len <1) || (text_len < 1) then
                Interrupted 0
        else
                search pattern pattern_len text text_len 0 0

let check_if_file s1 s2 =
        () (*TODO: add functionality for text files as well. returns 1 if file, 0 if string. *)


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
