%{
  (* Ocaml code here*)
open Ast

%}

(**************
 * The tokens *
 **************)

(* enter tokens here, they should begin with %token *)
%token SWAP        
%token  POP  
%token <int> PUSH 
%token  ADD  
%token  MUL 
%token  SUB
%token  DIV 
%token  REM  
%token <int> INT 
%token EOF

(******************************
 * Entry points of the parser *
 ******************************)

(* enter your %start clause here *)
%start <Ast.program> program
%type < Ast.command list > command  
%type <Ast.command> operator 

%%

(*************
 * The rules *
 *************)
program: i=INT EOF { i,[] }
program : 
| i=INT c=command EOF  {(i,c)}
command : 
  | o=operator c=command {o::c} 
  | o=PUSH c=command {(Push o)::c }
  | o=operator {[o]} 
  | o=PUSH  {[Push o]} 
operator :      
| POP {Pop} 
|ADD   {Add}  
| MUL   {Mul}
| SUB   {Sub}
| DIV   {Div} 
|REM   {Rem}  
| SWAP {Swap}

(* list all rules composing your grammar; obviously your entry point has to be present *)