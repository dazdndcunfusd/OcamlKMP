type result = (* types help handling whether success or failure *)
  | Found of int       (* the offset k where a match lies *)
  | Interrupted of int (* the state j that was reached *)

let assertInterrupted = function
  | Found _       -> assert false
  | Interrupted j -> j

let print_result result =
        match result with 
        | Found x -> print_string "found result at character ";print_int (x + 1) ; print_newline ()
        | Interrupted x -> print_string "Error at character ";print_int (x + 1) ; print_newline ()

(* Brute force Implementation -- NOT KMP *)
let rec search pattern lpattern text ltext table j k =
        if j = lpattern then
                Found (k - j)
        else if k = ltext then
                Interrupted j
        else if pattern.[j] = text.[k] then
                search pattern lpattern text ltext table (j + 1) (k + 1)
        else if j = 0 then
                search pattern lpattern text ltext table 0 (k+1)
        else
                search pattern lpattern text ltext table table.(j) k

        (* lots of backtracking. O(m*(n-1)) AKA terrible *)
        (* l, j, and k are all iterators, keeping track of the last known position. *)

(*pre-processing the table *)
let init pattern m = 
        (*TODO : change Array to a different type of handling. Maybe a list? Make a list of all zeros and use an iterator? *)
        let table = Array.make m 0 in
        for j = 2 to m-1 do
                table.(j) <- assertInterrupted (
                search pattern m pattern j table table.(j - 1) (j -1)
                )
        done;
        table

     

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
        let table = init pattern pattern_len in
        let result = search pattern pattern_len text text_len table 0 0 in
        print_result result
