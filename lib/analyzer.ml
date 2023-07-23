open Language

module type Domain = sig
  type abs_val
  (** Defines the abstract value type.
      Its definition is specific to the chosen value domain *)

  val val_bot : abs_val
  val val_top : abs_val
  val val_cnst : const -> abs_val
  val val_incl : abs_val -> abs_val -> bool
  val val_join : abs_val -> abs_val -> abs_val
  val val_uop : uop -> abs_val -> abs_val
  val val_binop : bop -> abs_val -> abs_val -> abs_val
  val val_sat : rel -> const -> abs_val -> abs_val
  val nr_is_le : abs_val array -> abs_val array -> bool
  val nr_bot : abs_val array -> abs_val array
  val nr_is_bot : abs_val array -> bool
  val nr_join : abs_val array -> abs_val array -> abs_val array
  val print_abs_val : int -> abs_val -> unit
end

(* The Analyzer module is a functor, i.e., a module parametrized by the analysis domain to be used. *)
module Analyzer (Dom : Domain) = struct
  (**Instantiate the print function for the analysis domain *)
  let print_abs_val idx arr = Dom.print_abs_val idx arr.(idx)

  (** Analysis of expressions in the abstract domain *)
  let rec abs_expr expr aenv =
    match expr with
    | Const c -> Dom.val_cnst c
    | Var x -> read_mem x aenv
    | Bop (op, e1, e2) -> Dom.val_binop op (abs_expr e1 aenv) (abs_expr e2 aenv)
    | Uop (op, e) -> Dom.val_uop op (abs_expr e aenv)

  (** Analysis of conditionals in the abstract domain *)
  let abs_cond (rel, var, cnst) aenv =
    print_cond (rel, var, cnst);
    let new_aval = Dom.val_sat rel cnst (read_mem var aenv) in
    if new_aval = Dom.val_bot then Dom.nr_bot aenv
    else write_mem var new_aval aenv

  (** Helper function to negate cond, language-dependent *)
  let negate_cond cond =
    match cond with
    | Lt, var, cnst -> (Ge, var, cnst)
    | Le, var, cnst -> (Gt, var, cnst)
    | Gt, var, cnst -> (Le, var, cnst)
    | Ge, var, cnst -> (Lt, var, cnst)

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
