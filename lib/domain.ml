open Language

module type Domain = sig
  type abs_val
  (** Defines the abstract value type.
      Its definition is specific to the chosen value domain *)

  val val_bot : abs_val
  val val_top : abs_val
  val val_cnst : Language.value -> abs_val
  val val_incl : abs_val -> abs_val -> bool
  val val_join : abs_val -> abs_val -> abs_val
  val val_uop : uop -> abs_val -> abs_val
  val val_binop : bop -> abs_val -> abs_val -> abs_val
  val val_sat : rel -> Language.value -> abs_val -> abs_val
  val nr_is_le : abs_val Memory.t -> abs_val Memory.t -> bool
  val nr_bot : abs_val Memory.t -> abs_val Memory.t
  val nr_is_bot : abs_val Memory.t -> bool
  val nr_join : abs_val Memory.t -> abs_val Memory.t -> abs_val Memory.t
  val print_abs_val : abs_val -> unit
end
