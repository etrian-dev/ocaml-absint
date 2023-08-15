open Language
open Domain

(* The Analyzer module is a functor, i.e., a module parametrized by the analysis domain to be used. *)
module Analyzer (Dom : Domain) = struct
  (**Instantiate the print function for the analysis domain *)
  let print_abs_val value = Dom.print_abs_val value

  (** Analysis of expressions in the abstract domain *)
  let rec abs_expr expr aenv =
    match expr with
    | Const c -> Dom.val_cnst (Int c)
    | Var x -> read_mem (Stack x) aenv
    | Bop (op, e1, e2) -> Dom.val_binop op (abs_expr e1 aenv) (abs_expr e2 aenv)
    | Uop (op, e) -> Dom.val_uop op (abs_expr e aenv)
    | Malloc site ->
        let k = new_heap_addr site aenv in
        read_mem k (write_mem k Dom.val_top aenv)
    | Ref _ -> Dom.val_top
    | Deref x -> read_mem x aenv

  (** Analysis of conditionals in the abstract domain *)
  let abs_cond (rel, v0, v1) aenv =
    match (v0, v1) with
    | Int _, Address a ->
        let new_aval = Dom.val_sat rel v0 (read_mem a aenv) in
        if new_aval = Dom.val_bot then Dom.nr_bot aenv
        else write_mem a new_aval aenv
    | _, _ -> aenv

  (** Helper function to negate cond, language-dependent *)
  let negate_cond cond =
    match cond with
    | Lt, var, cnst -> (Ge, var, cnst)
    | Le, var, cnst -> (Gt, var, cnst)
    | Gt, var, cnst -> (Le, var, cnst)
    | Ge, var, cnst -> (Lt, var, cnst)
    | Eq, var, cnst -> (Ne, var, cnst)
    | Ne, var, cnst -> (Eq, var, cnst)

  (** Analysis of commands, language-dependent *)
  let rec abs_command cmd pre =
    match cmd with
    | Skip -> pre
    | Seq ((_, c1), (_, c2)) -> abs_command c2 (abs_command c1 pre)
    | Assign (var, expr) ->
        let new_aenv = write_mem var (abs_expr expr pre) pre in
        if Dom.nr_is_bot new_aenv then Dom.nr_bot new_aenv else new_aenv
    | Input _var -> Dom.nr_bot pre (* TODO *)
    | If (cond, (_, then_cmd), (_, else_cmd)) ->
        let then_aenv = abs_command then_cmd (abs_cond cond pre) in
        let else_aenv =
          abs_command else_cmd (abs_cond (negate_cond cond) pre)
        in
        Dom.nr_join then_aenv else_aenv
    | While (cond, (_, c)) ->
        (* refine the cond *)
        let cond_aenv = abs_cond cond pre in
        if Dom.nr_is_bot cond_aenv then abs_cond (negate_cond cond) pre
        else
          (* Get the next abstract iterate *)
          let next_aenv = abs_command c cond_aenv in
          (* If next <=# current then the lfp has been reached *)
          if Dom.nr_is_le next_aenv pre then
            (* Abstract join with the negated loop cond *)
            Dom.nr_join cond_aenv pre
          else
            (* Otherwise keep computing iterates *)
            abs_command cmd (Dom.nr_join next_aenv pre)
end

module Interval_Analyzer = Analyzer (Intervals)
module Signs_Analyzer = Analyzer (Signs)
