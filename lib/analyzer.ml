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
  if new_aval = val_bot then nr_bot aenv else write_mem var new_aval aenv

let negate_cond cond =
  match cond with
  | Rle, var, cnst -> (Rgt, var, cnst)
  | Rgt, var, cnst -> (Rle, var, cnst)

(* Analysis of commands *)
let rec abs_command cmd pre =
  match cmd with
  | Skip -> pre
  | Seq ((_, c1), (_, c2)) -> abs_command c2 (abs_command c1 pre)
  | Assign (var, expr) ->
      let new_aenv = write_mem var (abs_expr expr pre) pre in
      if nr_is_bot new_aenv then nr_bot new_aenv else new_aenv
  | Input _var -> nr_bot pre (* TODO *)
  | If (cond, (_, then_cmd), (_, else_cmd)) ->
      let then_pre = abs_cond cond pre in
      let then_aenv = abs_command then_cmd then_pre in
      let else_pre = abs_cond (negate_cond cond) pre in
      let else_aenv = abs_command else_cmd else_pre in
      (*Printf.printf "\nThen branch pre:\n";
        Array.iteri print_abs_val then_pre;
        Printf.printf "\nElse branch pre:\n";
        Array.iteri print_abs_val else_pre;
        Printf.printf "\nThen branch post:\n";
        Array.iteri print_abs_val then_aenv;
        Printf.printf "\nElse branch post:\n";
        Array.iteri print_abs_val else_aenv;*)
      nr_join then_aenv else_aenv
  | While (cond, (_, cmd)) ->
      (* refine the cond *)
      let cond_aenv = abs_cond cond pre in
      (* Get the next abstract iterate *)
      let next_aenv = abs_command cmd cond_aenv in
      (* If next <=# current then the lfp has been reached *)
      if nr_is_le next_aenv pre (* Abstract join with the negated loop cond *)
      then nr_join (abs_cond (negate_cond cond) next_aenv) pre
        (* Otherwise keep computing iterates *)
      else abs_command cmd (nr_join next_aenv pre)
