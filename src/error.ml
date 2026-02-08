(*
 * Since:   March 15, 2017
 * Author:  Constantin
 *
 * Manage location and error
 *)

open Printf
open Lexing


(* -------------------------------------------------------------------------- *)
(* Location *)
(* -------------------------------------------------------------------------- *)
type location = string * int * int  (* File, Line, Col  *)

(* Print location in a pretty ugly way *)
let print_location (fname, lineno, charpos) =
    print_string fname;
    print_string " / ";
    print_string (string_of_int lineno);
    print_string " / ";
    print_string (string_of_int charpos);
    ;;

(* Return the current location *)
let current_loc =
    let pos     = Parsing.symbol_start_pos() in
    let fname   = pos.pos_fname in
    let lineno  = pos.pos_lnum in
    let charpos = pos.pos_cnum - pos.pos_bol in
    (fname, lineno, charpos);;


(* -------------------------------------------------------------------------- *)
(* Error *)
(* -------------------------------------------------------------------------- *)
exception Error of (location * string)

(* Called by the parser function on error *)
let parse_error msg = 
    flush stdout;;

(* Print error *)
let print_error loc msg =
    print_string "[ERR] ";
    print_location loc;
    print_string ": Parse error: ";
    print_endline msg;;
    flush stdout;;

(* Print expression error *)
let print_exp_error loc msg = 
    print_string "[ERR] ";
    print_location loc;
    print_string ": ";
    print_endline msg;;

let lexbuf_curr_loc lexbuf = 
    let fname   = lexbuf.lex_curr_p.pos_fname in
    let lineno  = lexbuf.lex_curr_p.pos_lnum in
    let charpos = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
    (fname, lineno, charpos);;

(* Throw an error *)
let tosserr lexbuf msg =
    let errmsg  = "Lexer error: "^msg in
    let fname   = lexbuf.lex_curr_p.pos_fname in
    let lineno  = lexbuf.lex_curr_p.pos_lnum in
    let charpos = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
    let loc     = (fname, lineno, charpos) in
    raise (Error(loc, errmsg))

let print_parser_err lexbuf =
    let loc     = lexbuf_curr_loc lexbuf in
    let str     = Lexing.lexeme lexbuf in
    let errmsg  = "Parser error around token "^str in
    print_exp_error loc errmsg;;
