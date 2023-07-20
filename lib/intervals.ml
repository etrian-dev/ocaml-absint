(* Integer intervals domain, with +inf and -inf *)

open Language

type abs_val = Interval of bound * bound
and bound = Val of int | Inf_Pos | Inf_Neg

(* The empty interval is represented by [+inf,-inf],
   out of all possible empty intervals. This value is
   used for normalization. *)
let val_bot = Interval (Inf_Pos, Inf_Neg)
let val_top = Interval (Inf_Neg, Inf_Pos)
let val_cnst c = Interval (Val c, Val c)

let le_ext a b =
  match (a, b) with
  | Inf_Neg, _ | _, Inf_Pos -> true
  | Inf_Pos, _ | _, Inf_Neg -> false
  | Val va, Val vb -> va <= vb

let min a b = if le_ext a b then a else b
let max a b = if le_ext a b then b else a

(* Implements the test [l_a, h_a] <= [l_b, h_b] *)
let val_incl (Interval (l_a, h_a)) (Interval (l_b, h_b)) =
  if le_ext l_b l_a && le_ext h_a h_b then true else false

(* Join with normalization (i.e., map any empty interval to val_bot defined above) *)
let val_join a b =
  if val_incl a b then b
  else
    let (Interval (l_a, h_a)) = a in
    let (Interval (l_b, h_b)) = b in
    Interval (min l_a l_b, max h_a h_b)

let op_ext op (Interval (l_a, h_a)) (Interval (l_b, h_b)) =
  let aux op a b =
    match (a, b) with
    | Inf_Neg, _ | _, Inf_Neg -> Inf_Neg
    | _, Inf_Pos | Inf_Pos, _ -> Inf_Pos
    | Val va, Val vb -> Val (op va vb)
  in
  Interval (aux op l_a l_b, aux op h_a h_b)

let val_binop op a b =
  match op with
  | Add -> op_ext ( + ) a b
  | Sub -> op_ext ( - ) a b
  | Mul -> op_ext ( * ) a b

let val_sat cond cnst (Interval (l, h)) =
  match cond with
  | Rle ->
      if le_ext (Val cnst) l then val_bot else Interval (l, min (Val cnst) h)
  | Rgt ->
      if le_ext h (Val cnst) then val_bot else Interval (max (Val cnst) l, h)

(* Decide whether the abstract env1 <=# env2, i.e., for all (a,b) in abs1 x abs2. a <=# b *)
let nr_is_le aenv1 aenv2 = Array.for_all2 val_incl aenv1 aenv2

(* Abstract join operation on envs, producing a new abstract env *)
let nr_join aenv1 aenv2 = Array.map2 val_join aenv1 aenv2

(* Tests whether the abstract env describes the empty set of stores, that is,
   at least one of its variables is bottom *)
let nr_is_bot aenv = Array.exists (fun a -> a = val_bot) aenv

(* Brings an abstract env to bottom *)
let nr_bot aenv = Array.map (fun _a -> val_bot) aenv

let print_abs_val idx (Interval (low, hi)) =
  let print_val_ext a =
    match a with Inf_Neg -> "-∞" | Inf_Pos -> "+∞" | Val v -> string_of_int v
  in
  let s = "[" ^ print_val_ext low ^ ", " ^ print_val_ext hi ^ "]" in
  Printf.printf "amem[%d] = %s\n" idx s

let _ = print_abs_val 0 (Interval (Val 2, Val 3))
