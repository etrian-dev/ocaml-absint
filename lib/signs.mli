type abs_val = Atop | Abot | Aneg | Apos

val val_bot : abs_val
val val_top : abs_val
val val_cnst : int -> abs_val
val val_incl : abs_val -> abs_val -> bool
val val_join : abs_val -> abs_val -> abs_val
val val_uop : Language.uop -> abs_val -> abs_val
val val_binop : Language.bop -> abs_val -> abs_val -> abs_val
val val_sat : Language.rel -> int -> abs_val -> abs_val
val nr_is_le : abs_val array -> abs_val array -> bool
val nr_join : abs_val array -> abs_val array -> abs_val array
val nr_is_bot : abs_val array -> bool
val nr_bot : 'a array -> abs_val array
val print_abs_val : int -> abs_val -> unit
val abs_eq : abs_val -> abs_val -> bool
