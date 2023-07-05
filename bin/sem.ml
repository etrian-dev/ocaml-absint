open Language

(* Semantic of a binary expression: a function bop -> const -> const -> const *)
let sem_bop op c1 c2 =
  match op with Add -> c1 + c2 | Sub -> c1 - c2 | Mul -> c1 * c2

(* Semantic of an expression: a function expr -> mem -> const *)
let rec sem_expr expr mem =
  match expr with
  | Const c -> c
  | Var v -> read_mem v mem
  | Bop (op, lhs, rhs) -> sem_bop op (sem_expr lhs mem) (sem_expr rhs mem)

(* Semantic of a relation: a function rel -> expr -> expr -> bool *)
let sem_rel rel v0 v1 = match rel with Rle -> v0 <= v1 | Rgt -> v0 > v1

(* Semantic of a condition: a function cond -> mem -> bool *)
let sem_cond (rel, var, const) mem = sem_rel rel (read_mem var mem) const

(* Semantic of a command: a function cmd -> mem -> mem *)
let rec sem_cmd cmd mem =
  match cmd with
  | Skip -> mem
  | Seq (c0, c1) -> sem_cmd c1 (sem_cmd c0 mem)
  | Assign (var, expr) -> write_mem var (sem_expr expr mem) mem
  | Input _ -> mem (* Scanf.scanf... *)
  | If (cond, (_, cmd1), (_, cmd2)) ->
      if sem_cond cond mem then sem_cmd cmd1 mem else sem_cmd cmd2 mem
  | While (cond, (_, com)) ->
      if sem_cond cond mem then sem_cmd cmd (sem_cmd com mem) else mem
