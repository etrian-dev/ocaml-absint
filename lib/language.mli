type label = int
type const = label
type var = const
type bop = Add | Sub | Mul | Div
type uop = Minus
type rel = Lt | Le | Gt | Ge
type heap_address = var * var
type address = Var of var | Heap of heap_address
type value = Int of var | Address of address

type expr =
  | Const of var
  | Var of var
  | Bop of bop * expr * expr
  | Uop of uop * expr

type cond = rel * var * var

type cmd =
  | Skip
  | Seq of com * com
  | Assign of var * expr
  | Input of var
  | If of cond * com * com
  | While of cond * com

and com = var * cmd

type mem = value array

val read_mem : int -> 'a array -> 'a
val write_mem : int -> 'a -> 'a array -> 'a array

type state = var * mem

val print_cond : rel * int * int -> unit
