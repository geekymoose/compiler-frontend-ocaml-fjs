# Compiler project
- April 14, 2017
- University of Montreal
- Compiler



# Project presentation
- Work for the Compiler lesson at University of Montreal.
- This project implements a simple frontend compiler for the FJS language
(Functional language, see description below)
- A Lexical analysis is done using ocamllex
- A Syntax analysis is done using ocamlyacc
- This process output the Abstract Syntax Tree (AST)



# Compile and run project
```
cd src
make
fjs-parser x
```
> x is a jfs file to parse



# FJS language
> Functional language inspired from JavaScript language.<br/>
> Introduced by University of Montreal for the Compiler course

## Syntax
#### Expressions
| Expression | Syntax |
| --- | --- |
| Anonymous function | function(arg1, arg2, ...) exp |
| Call function | expf(exp1, exp2, ...) |
| Label selection | exp.label |
| Test | if(test1) exp1 else if (test2) else exp3 |
| Blocks | {body} |
| Infixes operators | + - * / |
| Prefixes operators | + - |
| Comparison operators | < == |

#### Declarations
| Declaration | Syntax |
| --- | --- |
| Function | function id(arg1, arg2, ...) exp |
| Variable | var id = exp |

#### Other
- 'else' is optional
- A file contains a body
- body is a sequence of declarations and expressions separated by a semi-colon
- Can have comments (One lines / multi-lines)
- String with "..." (And common escape char with \\)



# Author
- Constantin MASSON ([GeekyMoose](https://github.com/GeekyMoose))
