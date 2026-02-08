/*
 * Since:   Feb 9, 2017
 * Author:  Constantin
 *
 * Parser for the UdeM fjs language grammar (Functional language)
 */

/* ---------------------------------------------------------------------------*/
/* HEADER SECTION */
/* ---------------------------------------------------------------------------*/
%{
    open Error;;
%}


/* ---------------------------------------------------------------------------*/
/* OCAMLYACC DECLARATIONS */
/* ---------------------------------------------------------------------------*/
%token <int> INT_VALUE
%token <string> STR_VALUE
%token <string> IDENTIFIER
%token <bool> BOOLEAN
%token PLUS MINUS STAR SLASH LT EQ
%token LPAR RPAR LBRACET RBRACET
%token PERIOD COMMA SEMICOLON UNDERSCORE
%token IF ELSE VAR FUNCTION
%token EOF

/* See binop rules: %left is not required anymore
%left PLUS MINUS
%left STAR SLAH
*/

%start program
%type <Exp.exp list> program


/* ---------------------------------------------------------------------------*/
/* GRAMMAR RULES (Rules and actions) */
/* ---------------------------------------------------------------------------*/
%%


/* -------------------------------------------------------------------------- */
/* Layers */
/* -------------------------------------------------------------------------- */
program:
    | body EOF {$1}
    /*| error EOF {print_error current_loc "Invalid program"; []}*/
;

body:
    | /*Empty body*/ {[]}
    | statement_list {$1}
;

block:
    | LBRACET body RBRACET {$2}
;


/* -------------------------------------------------------------------------- */
/* Statements - Expressions */
/* -------------------------------------------------------------------------- */
statement_list:
    | block {$1} /* Must be moved. here error if 2 blocks */
    | statement {[$1]}
    | statement_list statement {$1@[$2]}
;

statement:
    | declaration SEMICOLON {$1}
    | expression SEMICOLON {$1}
;

declaration:
    | function_declaration {$1}
    | variable_declaration {$1}
;

expression:
    | function_call {$1}
    | if_statement {$1}
    | string_value {$1}
    | binop {$1}
;


/* -------------------------------------------------------------------------- */
/* If-then-else */
/* -------------------------------------------------------------------------- */
/* TODO: fix bug: shift/reduce and reduce/reduce here */
if_statement:
    | IF LPAR if_test RPAR if_follow{
            Exp.If(current_loc, $3, $5, Exp.Num 0)
        }
    | IF LPAR if_test RPAR if_follow ELSE if_follow {
            Exp.If(current_loc, $3, $5, $7)
        }
;

if_follow:
    | IF LPAR if_test RPAR if_follow ELSE if_follow {
            Exp.If(current_loc, $3, $5, $7)
        }
    | LBRACET expression RBRACET {$2}
    | expression {$1}
;

if_test:
    unary_test {$1}
;


/* -------------------------------------------------------------------------- */
/* Binop operations
/* -------------------------------------------------------------------------- */
binop:
    | binop_factor binop_add {$2 $1}
;

binop_factor:
    | binop_final binop_factor_follow {$2 $1}
;

binop_factor_follow:
    | /*Empty*/ {fun x -> x}
    | STAR binop_factor {fun x -> Exp.PrimOp(current_loc, Exp.Mul, [x;$2])}
    | SLASH binop_factor {fun x -> Exp.PrimOp(current_loc, Exp.Div, [x;$2])}
;

binop_add:
    | /*Empty*/ {fun x -> x}
    | PLUS binop {fun x -> Exp.PrimOp(current_loc, Exp.Add, [x;$2])}
    | MINUS binop {fun x -> Exp.PrimOp(current_loc, Exp.Sub, [x;$2])}
;

binop_final:
    | number_value {$1}
    | variable_get {$1}
    | LPAR binop RPAR {$2}
    /*| MINUS expression {Exp.PrimOp(current_loc, Exp.Neg, [$2;])}*/
;


/* -------------------------------------------------------------------------- */
/* Unary tests
/* -------------------------------------------------------------------------- */
unary_test:
    | binop unary_test_follow {$2 $1}
;

unary_test_follow:
    | LT EQ expression {fun x -> Exp.PrimOp(current_loc, Exp.Leq, [x;$3])}
    | LT expression {fun x -> Exp.PrimOp(current_loc, Exp.Lt, [x;$2])}
    | EQ EQ expression {fun x -> Exp.PrimOp(current_loc, Exp.Eq, [x;$3])}
;


/* -------------------------------------------------------------------------- */
/* "Final" elements
/* -------------------------------------------------------------------------- */
number_value:
    | INT_VALUE {Exp.Num $1}
;

string_value:
    | STR_VALUE {Exp.Str $1}
;

identifier:
    | IDENTIFIER {let id = (current_loc, $1) in id}
;

boolean:
    | BOOLEAN {Exp.Boolean $1}
;


/* -------------------------------------------------------------------------- */
/* Variables */
/* -------------------------------------------------------------------------- */
variable_declaration:
    | VAR variable_declaration_list SEMICOLON expression {Exp.Let($2,$4)}
    | VAR variable_declaration_list SEMICOLON declaration {Exp.Let($2,$4)}
;

variable_declaration_list:
    | variable_declaration_element {[$1]}
    | variable_declaration_element COMMA variable_declaration_list {[$1]@$3}
;

variable_declaration_element:
    | identifier EQ expression {($1,$3)}
;

variable_get:
    | identifier {Exp.Var $1}
;


/* -------------------------------------------------------------------------- */
/* Functions
/* -------------------------------------------------------------------------- */
function_declaration:
    | FUNCTION identifier LPAR list_args_option RPAR expression {
            Exp.Function ($4, $6)
        }
;

list_args_option:
    | /* No args */ {[]}
    | list_args {$1}
;

list_args:
    | identifier {[$1]}
    | list_args COMMA identifier {$1@[$3]}
;

/* -------------------------------------------------------------------------- */
function_call:
    | expression LPAR list_parameters_option RPAR {
            Exp.Call (current_loc, $1, $3)
        }
;

list_parameters_option:
    | /* No params */ {[]}
    | list_parameters {$1}
;

list_parameters:
    | expression {[$1]}
    | list_parameters COMMA expression {$1@[$3]}
;


/* ---------------------------------------------------------------------------*/
/* TRAILER */
/* ---------------------------------------------------------------------------*/
%%
