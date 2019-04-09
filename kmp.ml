open Lazy
type pattern = { is_match : bool; step : char -> pattern}

let is_match = function
        |{is_match = true; step = _} -> true
        | _ -> false

let mk b f = {is_match = b; step = f}
let step c p = p.step c
let rec const b = {is_match = b; step = fun _ -> const b}
let rec to_list_ch = function
        |""-> []
        | ch -> (String.get ch 0 ) :: (to_list_ch ( String.sub ch 1 ( (String.length ch) -1 )))

let run ( pattern: pattern) (text : string) : int list =
        let accum ( pattern, ix, acc) c =
                let pattern = step c pattern in (*TODO: change step to next if issues *)
                let acc = if is_match pattern then (ix :: acc) else acc in
                (pattern, ix+1, acc)
        in
        let (_,_,acc) = List.fold_left accum (pattern, 0, []) (to_list_ch text) in 
        List.rev acc

let generate_pattern (cs: char list) : pattern =
        let rec pattern = lazy (gen pattern cs)
        and gen curr_pattern = function
                | [] -> const true
                | [c] -> mk false @@ fun x ->
                                let next_pattern = force curr_pattern in
                                if x = c then mk true (fun _ ->  next_pattern) (*TODO: fix this*)
                                else next_pattern
                |c :: cs ->
                                let next_pattern = lazy (step c @@ force curr_pattern) in
                                let cont_pattern = lazy (gen next_pattern cs) in
                                mk false @@ fun x -> force @@ if x = c then cont_pattern 
                                                              else curr_pattern
        in
        force pattern

let search cs = run @@ generate_pattern cs

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

     

let check_if_file s1=
        let is_txt file = Filename.check_suffix file ".txt" in
        if (is_txt s1) then true else false


let () = 
        let args_n = Array.length Sys.argv - 1 in
        let args_list = Array.to_list (Array.sub Sys.argv 1 args_n) in
        let pattern = List.nth args_list 0 in
        let text = List.nth args_list 1 in
        let pattern_len = String.length pattern in
        let text_len = String.length text in
        (* TODO: Write conditional if either s1 or s2 is a file, read into string for search *)
        let table = init pattern pattern_len in
        let result = search pattern pattern_len text text_len table 0 0 in
        print_result result;
