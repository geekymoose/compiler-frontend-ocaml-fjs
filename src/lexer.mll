(*
 * Since:   Feb 9, 2017
 * Author:  Constantin
 *
 * Lexer for the UdeM fjs language grammar (Functional language)
 *)


(* ---------------------------------------------------------------------------*)
(* HEADER SECTION *)
(* ---------------------------------------------------------------------------*)
{
    open Parser
    open Lexing
    open Error

    (* Update the current lexbuf position *)
    let incr_lineno lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- {
            pos with
            pos_lnum = pos.pos_lnum + 1;
            pos_bol = pos.pos_cnum;
        }
}


(* ---------------------------------------------------------------------------*)
(* DEFINITIONS SECTION *)
(* ---------------------------------------------------------------------------*)
let digit       = ['0'-'9']
let alpha_upper = ['A'-'Z']
let alpha_lower = ['a'-'z']
let alpha       = alpha_upper | alpha_lower
let alphanum    = digit | alpha

let identifier  = (alpha | '_') (alphanum | '_')*

let newline     = "\r\n" | "\n\r" | '\n' | '\r'
let whitespace  = ' '
let tabulation  = '\t'
let blank       = tabulation | whitespace



(* ---------------------------------------------------------------------------*)
(* RULES SECTION *)
(* ---------------------------------------------------------------------------*)
rule token = parse
    (* layout *)
    | newline       {incr_lineno lexbuf; token lexbuf}
    | blank+        {token lexbuf}

    (* operators *)
    | '+'           {PLUS}
    | '-'           {MINUS}
    | '*'           {STAR}
    | '/'           {SLASH}
    | '<'           {LT}
    | '='           {EQ}

    (* elements / punctuation *)
    | '('           {LPAR}
    | ')'           {RPAR}
    | '{'           {LBRACET}
    | '}'           {RBRACET}
    | '.'           {PERIOD}
    | ','           {COMMA}
    | ';'           {SEMICOLON}
    | '_'           {UNDERSCORE}

    (* keywords *)
    | "if"          {IF}
    | "else"        {ELSE}
    | "function"    {FUNCTION}
    | "var"         {VAR}
    | "true"        {BOOLEAN(true)}
    | "false"       {BOOLEAN(false)}

    (* special keywords *)
    | "/*"          {multilines_comments 0 lexbuf}
    | "//"          {inline_comments lexbuf}

    (* strings *)
    | '"'           {STR_VALUE (double_quoted_string (Buffer.create 16) lexbuf)}

    (* values *)
    | identifier as value   {IDENTIFIER (value)}
    | digit+ as value       {INT_VALUE (int_of_string value)}

    (* Special elements / Unrecognized elements*)
    | eof           {EOF}
    | _             {tosserr lexbuf "Unknown character."}


    (* multi-lines comments. Allow nested comments (No line restriction) *)
    and multilines_comments level = parse
    | "*/"          {if level = 0
                        then token lexbuf
                        else multilines_comments (level-1) lexbuf
                    }
    | "/*"          {multilines_comments (level+1) lexbuf}
    | _             {multilines_comments level lexbuf}
    | eof           {tosserr lexbuf "Comment started but not ended."}


    (* inline comments. Finish as soon as new line *)
    and inline_comments = parse
    | newline       {token lexbuf}
    | _             {inline_comments lexbuf}


    (* double_quoted_string *)
    and double_quoted_string buff = parse
    | '"'           {Buffer.contents buff;}
    | '\\'          {escape_string_elt buff double_quoted_string lexbuf}
    | eof           {tosserr lexbuf "String started but not ended."}
    | _ as c        {Buffer.add_char buff c; double_quoted_string buff lexbuf}

    and escape_string_elt buff return_fct = parse
    | '"'
    | '\''
    | '\\' as c     {Buffer.add_char buff c; return_fct buff lexbuf}
    | 'a'           {Buffer.add_char buff (char_of_int 7);
                        return_fct buff lexbuf}
    | 'b'           {Buffer.add_char buff (char_of_int 8);
                        return_fct buff lexbuf}
    | 'f'           {Buffer.add_char buff (char_of_int 12);
                        return_fct buff lexbuf}
    | 'n'           {Buffer.add_char buff (char_of_int 10);
                        return_fct buff lexbuf}
    | 'r'           {Buffer.add_char buff (char_of_int 13);
                        return_fct buff lexbuf}
    | 't'           {Buffer.add_char buff (char_of_int 9);
                        return_fct buff lexbuf}
    | 'v'           {Buffer.add_char buff (char_of_int 11);
                        return_fct buff lexbuf}





(* ---------------------------------------------------------------------------*)
(* TRAILER SECTION *)
(* ---------------------------------------------------------------------------*)
{
    (* DEBUG FUNCTION - Iterate through the tokens and display flow of tokens *)
    let rec debug_iter_tokens lexbuf =
        let tok = token lexbuf in
        let print_token t = print_string t;print_string " " in
        match tok with
        | PLUS          -> print_token "PLUS"; debug_iter_tokens lexbuf
        | MINUS         -> print_token "MINUS"; debug_iter_tokens lexbuf
        | STAR          -> print_token "STAR"; debug_iter_tokens lexbuf
        | SLASH         -> print_token "SLASH"; debug_iter_tokens lexbuf
        | LT            -> print_token "LT"; debug_iter_tokens lexbuf
        | EQ            -> print_token "EQ"; debug_iter_tokens lexbuf

        | LPAR          -> print_token "LPAR"; debug_iter_tokens lexbuf
        | RPAR          -> print_token "RPAR"; debug_iter_tokens lexbuf
        | LBRACET       -> print_token "LBRACET"; debug_iter_tokens lexbuf
        | RBRACET       -> print_token "RBRACET"; debug_iter_tokens lexbuf
        | PERIOD        -> print_token "PERIOD"; debug_iter_tokens lexbuf
        | COMMA         -> print_token "COMMA"; debug_iter_tokens lexbuf
        | SEMICOLON     -> print_endline "SEMICOLON"; debug_iter_tokens lexbuf
        | UNDERSCORE    -> print_token "UNDERSCORE"; debug_iter_tokens lexbuf

        | IF            -> print_token "IF"; debug_iter_tokens lexbuf
        | ELSE          -> print_token "ELSE"; debug_iter_tokens lexbuf
        | FUNCTION      -> print_token "FUNCTION"; debug_iter_tokens lexbuf
        | VAR           -> print_token "VAR"; debug_iter_tokens lexbuf

        | BOOLEAN x     ->  print_string "BOOL(";
                            print_string (string_of_bool x);
                            print_token ")"; 
                            debug_iter_tokens lexbuf

        | IDENTIFIER x  ->  print_string "IDENTIFIER(";
                            print_string x;
                            print_token ")";
                            debug_iter_tokens lexbuf
        | INT_VALUE x   ->  print_string "INT_VALUE(";
                            print_int x;
                            print_token ")";
                            debug_iter_tokens lexbuf
        | STR_VALUE x   ->  print_string "STR_VALUE(";
                            print_string x;
                            print_token ")";
                            debug_iter_tokens lexbuf

        | EOF           -> print_endline "EOF"
    ;;
}



