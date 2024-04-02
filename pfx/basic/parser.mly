%{
  (* OCaml code can be inserted here *)
%}

(**************
 * The tokens *
 **************)

(* Define the tokens recognized by the lexer *)
%token EOF                  (* End of file *)
%token <int> INT            (* Integer literal *)
%token PUSH                 (* Push command *)
%token POP                  (* Pop command *)
%token ADD                  (* Add command *)
%token SUB                  (* Subtract command *)
%token MUL                  (* Multiply command *)
%token DIV                  (* Divide command *)
%token REM                  (* Remainder command *)

(******************************
 * Entry points of the parser *
 ******************************)

(* Specify the entry point of the parser and the type of the resulting AST node *)
%start <Ast.program> program

%%

(*************
 * The rules *
 *************)

(* Define grammar rules *)

(* Represents the overall structure of a Pfx program *)
program:
  | INT EOF { $1, [] }     (* A program consists of an integer followed by end-of-file *)

(* Represents a list of commands in the Pfx program *)
command_list:
  | /* Empty */             (* An empty command list *)
  | command command_list   (* A command followed by another command list *)

(* Represents individual commands in the Pfx language *)
command:
  | PUSH INT                (* Push command followed by an integer literal *)
  | POP                     (* Pop command *)
  | ADD                     (* Add command *)
  | SUB                     (* Subtract command *)
  | MUL                     (* Multiply command *)
  | DIV                     (* Divide command *)
  | REM                     (* Remainder command *)

%%
