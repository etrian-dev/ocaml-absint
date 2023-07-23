type label = int
type const = int
type var = int
type bop = Add | Sub | Mul | Div
type uop = Minus
type rel = Lt | Le | Gt | Ge
type heap_address = int * int
type address = Var of var | Heap of heap_address
type value = Int of int | Address of address

type expr =
  | Const of const
  | Var of var
  | Bop of bop * expr * expr
  | Uop of uop * expr

type cond = rel * var * const

type cmd =
  | Skip
  | Seq of com * com
  | Assign of var * expr
  | Input of var
  | If of cond * com * com
  | While of cond * com

and com = label * cmd

type mem = value array

(* read_mem: var -> mem -> const *)
let read_mem var mem = mem.(var)

(* write_mem: var -> const -> mem -> mem *)
let write_mem var value mem =
  let new_mem = Array.copy mem in
  new_mem.(var) <- value;
  new_mem

type state = label * mem

let print_cond (rel, var, cnst) =
  let rel_str = if rel = Le then "<=" else ">=" in
  Printf.printf "x_%d %s %d" var rel_str cnst
