open Printf
open Error
module P = Parser
module E = Exp


(* ---------------------------------------------------------------------------*)
let input_file = 
    try open_in Sys.argv.(1)
    with Invalid_argument a
        -> print_string "No file name given!\n"; exit 1

let debug_print_env_elt key value =
    print_string key;;

let debug_print_env_list env =
    E.SMap.iter debug_print_env_elt env;;


(* ---------------------------------------------------------------------------*)
let eval_exp expression =
    try
        let value = E.eval E.env_init expression in
        print_string ("Result = " ^ (E.vprint value) ^ "\n")
    with Error ((file, line, col), name)
        -> print_string (file
                        ^":"^string_of_int line
                        ^":"^string_of_int col
                        ^":Error: "^name ^"\n")
    ;;

let rec eval_all_exp = function
    | [] -> ()
    | h::t -> eval_exp h; eval_all_exp t;
    ;;


(* ---------------------------------------------------------------------------*)
(* Main executin *)
(* ---------------------------------------------------------------------------*)
let debug_mode() =
    print_endline "\n----- DEBUG PARSE -----";
    let lexbuf = Lexing.from_channel input_file in
    Lexer.debug_iter_tokens lexbuf;
    print_endline "----- ----- -----";
    ;;

let parser_mode() =
    let lexbuf  = Lexing.from_channel input_file in
    try
        print_endline "\n----- PARSER -----";
        print_endline " - Start parsing...";
        let ast     = Parser.program Lexer.token lexbuf in
        let size    = List.length ast in
        print_string " + [SUCCESS] Parsing successfully done! (Number elt: ";
        print_int size;
        print_endline ")";
        print_endline "----- ----- -----";
        print_endline "\n----- EVAL -----";
        eval_all_exp ast;
        print_endline "----- ----- -----";
        print_endline "\n----- DEBUG Env list-----";
        debug_print_env_list E.env_init
    with
        | Parsing.Parse_error ->
            print_parser_err lexbuf
        | Error (loc,msg) ->
            print_exp_error loc msg
    ;;

let main() = 
    print_endline "\n----- START -----";
    (*debug_mode();*)
    parser_mode();
    print_endline "\n----- STOP -----\n";
    ;;

let _ = Printexc.print main()
