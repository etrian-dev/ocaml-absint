type label = int
type const = int
type var = int
type bop = Add | Sub | Mul
type rel = Rle | Rgt
type expr = Const of const | Var of var | Bop of bop * expr * expr
type cond = rel * var * const

type cmd =
  | Skip
  | Seq of com * com
  | Assign of var * expr
  | Input of var
  | If of cond * com * com
  | While of cond * com

and com = label * cmd

type mem = const array

(* read_mem: var -> mem -> const *)
let read_mem var mem = mem.(var)

(* write_mem: var -> const -> mem -> mem *)
let write_mem var c mem =
  let new_mem = Array.copy mem in
  new_mem.(var) <- c;
  new_mem

type state = label * mem
