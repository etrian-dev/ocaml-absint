(* Signs abstract domain *)

open Language

type abs_val = Atop | Abot | Aneg | Apos

let val_bot = Abot
let val_top = Atop

(* Maps a constant to an abstract element *)
let val_cnst value =
  match value with Int x -> if x >= 0 then Apos else Aneg | _ -> Atop

(* Checks whether the abstract value a0 <=# a1 *)
let val_incl a0 a1 =
  match (a0, a1) with
  | Abot, _ | _, Atop | Apos, Apos | Aneg, Aneg -> true
  | _ -> false

(* tests *)
let%test "⊥ <= ⊥" = val_incl Abot Abot
let%test "Apos <= ⊤" = val_incl Apos Atop
let%test "Aneg <= Apos " = not (val_incl Aneg Apos)
let%test "Apos <= Const 10" = val_incl Apos (val_cnst (Int 10))

let%test "Atop <= Address (Stack x)" =
  val_incl Atop (val_cnst (Address (Stack 1)))

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
  | Mul | Div -> (
      match (a0, a1) with
      | Apos, Apos -> Apos
      | Aneg, Aneg -> Apos
      | Apos, Aneg | Aneg, Apos -> Aneg
      | Abot, _ | _, Abot -> Abot
      | _ -> Atop)

let val_uop op a =
  match (op, a) with Minus, Apos -> Aneg | Minus, Aneg -> Apos | _, _ -> a

(* Taking into account condition expressed as a pair cond-const, refines
   the information of abs, producing abs' *)
let val_sat cond value abs =
  match (abs, value) with
  | Abot, _ -> Abot
  | Apos, Int v ->
      if (cond = Le && v < 0) || (cond = Lt && v <= 0) then Abot else Apos
  | Aneg, Int v ->
      if (cond = Ge && v > 0) || (cond = Gt && v >= 0) then Abot else Aneg
  | Atop, Int v ->
      if cond = Le && v <= 0 then Aneg
      else if cond = Gt && v >= 0 then Apos
      else Atop
  (* No info gained if the condition is about an address *)
  | _, _ -> abs

(* Decide whether the abstract env1 <=# env2, i.e., for all (a,b) in abs1 x abs2. a <=# b *)
let nr_is_le aenv1 aenv2 =
  Memory.compare (fun v1 v2 -> if val_incl v1 v2 then -1 else 1) aenv1 aenv2
  == -1

(* Abstract join operation on envs, producing a new abstract env *)
let nr_join aenv1 aenv2 =
  Memory.union (fun _k v1 v2 -> Some (val_join v1 v2)) aenv1 aenv2

(* Tests whether the abstract env describes the empty set of stores, that is,
   at least one of its variables is bottom *)
let nr_is_bot aenv = Memory.exists (fun _k v -> v = val_bot) aenv

(* Brings an abstract env to bottom *)
let nr_bot aenv = Memory.map (fun _a -> val_bot) aenv

let print_abs_val a =
  let s =
    match a with Atop -> "⊤" | Abot -> "⊥" | Apos -> "Apos" | Aneg -> "Aneg"
  in
  Printf.printf "%s\n" s

let abs_eq a b =
  match (a, b) with
  | Atop, Atop | Abot, Abot | Apos, Apos | Aneg, Aneg -> true
  | _ -> false

let%test "abs eq 1" = abs_eq Abot Abot
let%test "abs eq 2" = abs_eq Apos (val_cnst (Int 10))

(* TODO: tests for other operators as well *)
