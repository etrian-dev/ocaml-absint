(* Signs abstract domain *)

open Language

type abs_val = Atop | Abot | Aneg | Apos

let val_bot = Abot
let val_top = Atop

type nr_abs = abs_val array

(* Maps a constant to an abstract element *)
let val_cnst c = if c < 0 then Aneg else Apos

(* Checks whether the abstract value a0 <=# a1 *)
let val_incl a0 a1 =
  match (a0, a1) with
  | Abot, _ | _, Atop | Apos, Apos | Aneg, Aneg -> true
  | _ -> false

(* Abstract join operation *)
let val_join a0 a1 =
  match (a0, a1) with
  | Abot, _ | _, Abot -> Abot
  | Aneg, Aneg -> Aneg
  | Apos, Apos -> Apos
  | _ -> Atop

let val_binop op a0 a1 =
  match op with
  | Add -> (
      match (a0, a1) with
      | Apos, Apos -> Apos
      | Abot, _ | _, Abot -> Abot
      | _ -> Atop)
  | Sub -> (
      match (a0, a1) with
      | Aneg, Aneg -> Aneg
      | Abot, _ | _, Abot -> Abot
      | _ -> Atop)
  | Mul -> (
      match (a0, a1) with
      | Apos, Apos -> Apos
      | Aneg, Aneg -> Apos
      | Apos, Aneg | Aneg, Apos -> Aneg
      | Abot, _ | _, Abot -> Abot
      | _ -> Atop)

(* Taking into account condition expressed as a pair cond-const, refines
   the information of abs, producing abs' *)
let val_sat cond cnst abs =
  match abs with
  | Abot -> Abot
  | Apos -> if cond = Rle && cnst < 0 then Abot else Apos
  | Aneg -> if cond = Rgt && cnst > 0 then Abot else Aneg
  | Atop ->
      if cond = Rle && cnst <= 0 then Aneg
      else if cond = Rgt && cnst >= 0 then Apos
      else Atop

(* Decide whether the abstract env1 <=# env2, i.e., for all (a,b) in abs1 x abs2. a <=# b *)
let nr_is_le aenv1 aenv2 = Array.for_all2 val_incl aenv1 aenv2

(* Abstract join operation on envs, producing a new abstract env *)
let nr_join aenv1 aenv2 = Array.map2 val_join aenv1 aenv2

(* Tests whether the abstract env describes the empty set of stores, that is,
   at least one of its variables is bottom *)
let nr_is_bot aenv = Array.exists (fun a -> a = Abot) aenv

(* Brings an abstract env to bottom *)
let nr_bot aenv = Array.map (fun _a -> Abot) aenv

let print_abs_val idx a =
  let s =
    match a with Atop -> "⊤" | Abot -> "⊥" | Apos -> "Apos" | Aneg -> "Aneg"
  in
  Printf.printf "amem[%d] = %s\n" idx s

let abs_eq a b =
  match (a, b) with
  | Atop, Atop | Abot, Abot | Apos, Apos | Aneg, Aneg -> true
  | _ -> false

let%test "⊥ <= ⊥" = val_incl Abot Abot
let%test "Apos <= ⊤" = val_incl Apos Atop
let%test "Aneg <= Apos " = not (val_incl Aneg Apos)
let%test "Apos <= Const 10" = val_incl Apos (val_cnst 10)
let%test "abs eq 1" = abs_eq Abot Abot
let%test "abs eq 2" = abs_eq Apos (val_cnst 10)

(* TODO: tests for other operators as well *)
