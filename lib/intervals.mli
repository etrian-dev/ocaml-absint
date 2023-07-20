type abs_val = Interval of bound * bound
and bound = Val of int | Inf_Pos | Inf_Neg

val val_bot : abs_val
val val_top : abs_val
val val_cnst : int -> abs_val
val le_ext : bound -> bound -> bool
val min : bound -> bound -> bound
val max : bound -> bound -> bound
val val_incl : abs_val -> abs_val -> bool
val val_join : abs_val -> abs_val -> abs_val
val op_ext : (int -> int -> int) -> abs_val -> abs_val -> abs_val
val val_binop : Language.bop -> abs_val -> abs_val -> abs_val
val val_sat : Language.rel -> int -> abs_val -> abs_val
val nr_is_le : abs_val array -> abs_val array -> bool
val nr_join : abs_val array -> abs_val array -> abs_val array
val nr_is_bot : abs_val array -> bool
val nr_bot : 'a array -> abs_val array
val print_abs_val : int -> abs_val -> unit
