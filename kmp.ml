open Lazy

let rec print_ch_ls = function
        | [] -> print_string "\n"
        | h::t -> print_char h; print_string " "; print_ch_ls t

(*Creates pattern structure that will keep track of next step in pattern *)
type pattern = { is_match : bool; step : char -> pattern}
let is_match = function
        |{is_match = true; step = _} -> true
        | _ -> false

let mk b f = {is_match = b; step = f}
let next c p = p.step c
let rec const b = {is_match = b; step = fun _ -> const b}

let rec to_list_ch = function
        |""-> []
        | ch -> (String.get ch 0 ) :: (to_list_ch ( String.sub ch 1 ( (String.length ch) -1 )))

 (*Makes a list that checks *)
let run ( pattern: pattern) (text : string) : int list =
        let accum ( pattern, ix, acc) c =
                let pattern = next c pattern in
                let acc = if is_match pattern then (ix :: acc) else acc in
                (pattern, ix+1, acc)
        in
        (*TODO: ask what (_,_,acc) means *)let (_,_,acc) = List.fold_left accum (pattern, 0, []) (to_list_ch text) in 
        List.rev acc

let generate_pattern (cs: char list) : pattern = (*returns pattern *)
        let rec pattern = lazy (gen pattern cs)
        and gen curr_pattern = function
                | [] -> const true
                | [c] -> mk false @@ fun x ->
                                let next_pattern = force curr_pattern in
                                if x = c then mk true (fun _ ->force_val curr_pattern)
                                else next_pattern
                |c :: cs ->
                                let next_pattern = lazy (next c @@ force curr_pattern) in
                                let cont_pattern = lazy (gen next_pattern cs) in
                                mk false @@ fun x -> force @@ if x = c then cont_pattern 
                                                              else curr_pattern
        in
        force pattern

let search cs = run @@ generate_pattern cs
     
let check_if_file s1= (*TODO : if pattern is a txt file, pass it to be read and turned into a char list via file. otherwise call to_list_ch and return that *)
        (*TODO: if text is a file name, read file into string *)
        let is_txt file = Filename.check_suffix file ".txt" in
        if (is_txt s1) then true else false

let rec print_ls = function
        |[] -> print_string "\n"
        |h::[] -> print_int h;
                  print_string "\n";
        |h::t -> print_int h ; 
                 print_string ", ";
                 print_ls t

let () = 
        let args_n = Array.length Sys.argv - 1 in
        let args_list = Array.to_list (Array.sub Sys.argv 1 args_n) in
        let pattern = List.nth args_list 0 in
        let text = List.nth args_list 1 in
        (* TODO: Write conditional if either s1 or s2 is a file, read into string for search *)
        let int_ls = search (to_list_ch pattern) text in

        if (List.length int_ls)> 1 then print_string "pattern found at characters "
        else print_string "Pattern found at character ";
        print_ls int_ls;;

(*Where I can improve:
        * 1)pattern.step's char requirement is uneeded, and is allows simplification of code.
        * However, it causes additional running time when changing a large bit of text into 
        * a char list. 
        *2) Lazy.force is NOT thread safe. Therefore, locks would be needed if use on multicore.
        *)
(*Sources: 
        * https://www.brics.dk/RS/02/32/BRICS-RS-02-32.pdf
        * https://rlc.vlinder.ca/blog/2014/02/a-functional-version-of-the-kmp-algorithm/
        * http://jobjo.github.io/2015/07/27/Lazy-KMP-OCaml.html
        * *)
