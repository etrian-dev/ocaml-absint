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

val read_mem : int -> 'a array -> 'a
val write_mem : int -> 'a -> 'a array -> 'a array

type state = label * mem
