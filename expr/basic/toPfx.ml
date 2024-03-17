open Ast

let rec generate_expr = function
  | Const n -> [Push n]
  | Binop (op, e1, e2) ->
    let cmds_e1 = generate_expr e1 in
    let cmds_e2 = generate_expr e2 in
    cmds_e1 @ cmds_e2 @
    (match op with
    | BinOp.Badd -> [Add]
    | BinOp.Bsub -> [Sub]
    | BinOp.Bmul -> [Mul]
    | BinOp.Bdiv -> [Div]
    | BinOp.Bmod -> [Rem])
  | Uminus e ->
    let cmds_e = generate_expr e in
    cmds_e @ [Push 0; Swap; Sub]
  | Var _ -> failwith "Not yet supported"

let generate = function
  | Const _ as e -> (0, generate_expr e)
  | Binop(_,_,_) as e -> (0, generate_expr e)
  | Uminus _ as e -> (0, generate_expr e)
  | Var _ as e -> failwith "Not yet supported"
