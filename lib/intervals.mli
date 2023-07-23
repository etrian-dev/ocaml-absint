(** An interval bound: can be either a finite integer or infinite. *)
type bound = Val of int | Inf_Pos | Inf_Neg

(** The abstract element representing an interval of values on a variable.*)
type abs_val = Interval of bound * bound

(** The (normalized) empty interval: (+oo, -oo) *)
val val_bot : abs_val
(** The widest possible interval on a variable: (-oo, +oo) *)
val val_top : abs_val
(** Abstraction function of a constant [c] into the interval [[c, c]] *)
val val_cnst : int -> abs_val
(** Abstract inclusion relation between two intervals. *)
val val_incl : abs_val -> abs_val -> bool
val val_join : abs_val -> abs_val -> abs_val
val op_ext : (int -> int -> int) -> abs_val -> abs_val -> abs_val
val val_uop : Language.uop -> abs_val -> abs_val
val val_binop : Language.bop -> abs_val -> abs_val -> abs_val
val val_sat : Language.rel -> int -> abs_val -> abs_val
val nr_is_le : abs_val array -> abs_val array -> bool
val nr_join : abs_val array -> abs_val array -> abs_val array
val nr_is_bot : abs_val array -> bool
val nr_bot : 'a array -> abs_val array
val print_abs_val : int -> abs_val -> unit
