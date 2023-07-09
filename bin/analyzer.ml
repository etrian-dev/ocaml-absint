open Language
open Signs (* The analysis uses the signs abstract domain *)

(* Instantiate the print function for the analysis domain *)
let print_abs_val = Signs.print_abs_val


(* Analysis of expressions in the abstract domain *)
let rec abs_expr expr aenv =
  match expr with
  | Const c -> val_cnst c
  | Var x -> read_mem x aenv
  | Bop (op, e1, e2) -> val_binop op (abs_expr e1 aenv) (abs_expr e2 aenv)

(* Analysis of conditionals in the abstract domain *)
let abs_cond (rel, var, cnst) aenv =
  let new_aval = val_sat rel cnst (read_mem var aenv) in
  if new_aval = val_bot then nr_bot aenv
  else write_mem var new_aval aenv

(* Analysis of commands *)
let rec abs_command cmd pre =
  match cmd with
  | Skip -> pre
  | Seq ((_, c1), (_, c2)) -> abs_command c2 (abs_command c1 pre)
  | Assign (var, expr) ->
    let new_aenv = write_mem var (abs_expr expr pre) pre in
    if nr_is_bot new_aenv then nr_bot new_aenv else new_aenv
  | Input _var -> nr_bot pre (* TODO *)
  (* FIXME: analysis is correct only for simple cases *)
  | If (cond, (_, then_cmd), (_, else_cmd)) ->
    let cond_aenv = abs_cond cond pre in
    let then_aenv = abs_command then_cmd cond_aenv in
    let comp_cond = match cond with
      | (Rle, var, cnst) -> (Rgt, var, cnst)
      | (Rgt, var, cnst) -> (Rle, var, cnst) in
    let cond_alt_aenv = abs_cond comp_cond pre in
    let else_aenv = abs_command else_cmd cond_alt_aenv in
    nr_join then_aenv else_aenv
  | While (cond, (_, cmd)) ->
      let new_aenv = abs_cond cond pre in
      if nr_is_bot new_aenv then pre
      else
        let next_aenv = nr_join (abs_command cmd new_aenv) pre in
        if nr_is_le next_aenv pre then pre else abs_command cmd next_aenv