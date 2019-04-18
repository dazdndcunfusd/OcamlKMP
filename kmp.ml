open Lazy


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

(*checks against pattern *)
let run ( pattern: pattern) (text : string) (ln_pattern : int) : int list =
        let accum ( pattern, ix, acc) c =
                let pattern = next c pattern in
                let acc = if is_match pattern then ((ix - ln_pattern) :: acc) else acc in
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
                                if x = c then mk true (fun _ ->next_pattern)
                                else next_pattern
                |c :: cs ->
                                let next_pattern = lazy (next c @@ force curr_pattern) in
                                let cont_pattern = lazy (gen next_pattern cs) in
                                mk false @@ fun x -> force @@ if x = c then cont_pattern 
                                                              else curr_pattern
        in
        force pattern

let search cs text= run ( generate_pattern cs) text
     
let read_file file = 
        let ic = open_in file in
        try
                let line = input_line ic in
                let result = line in
                close_in ic;
                result
        with e->
                close_in_noerr ic;
                file
                             

let check_if_file s1=         
        let is_txt file = Filename.check_suffix file ".txt" in
        if (is_txt s1) then read_file s1 else s1

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
        let pattern =check_if_file(List.nth args_list 0 )in
        let lpattern = String.length pattern - 2 in
        let text = check_if_file(List.nth args_list 1) in
        let int_ls = search (to_list_ch pattern) text lpattern in
        if (List.length int_ls)> 1 then print_string "pattern found at characters "
        else if (List.length int_ls) = 1 then print_string "Pattern found at character "
        else print_string "No patterns found."
        ;
        print_string ("\n"^pattern^"\n");
        print_string (text^"\n");
        print_ls int_ls;;

(*Where I can improve:
        * 1)pattern.step's char requirement is uneeded, and is allows simplification of code.
        * However, it causes additional running time when changing a large bit of text into 
        * a char list. 
        *2) Lazy.force is NOT thread safe. Therefore, locks would be needed if use on multicore
        *3) when opening file, there is a limit to how long i can read. I would like to make that process more functional. current_limit is 192.
        *)
(*Sources: 
        * https://www.brics.dk/RS/02/32/BRICS-RS-02-32.pdf
        * https://rlc.vlinder.ca/blog/2014/02/a-functional-version-of-the-kmp-algorithm/
        * http://jobjo.github.io/2015/07/27/Lazy-KMP-OCaml.html
        * *)
