type label = int
type const = int
type var = int
type bop = Add | Sub | Mul | Div
type uop = Minus
type rel = Lt | Le | Gt | Ge | Eq | Ne
type site = int
type heap_address = site * int
type address = Stack of var | Heap of heap_address
type value = Int of int | Address of address

type expr =
  | Const of const
  | Var of var
  | Bop of bop * expr * expr
  | Uop of uop * expr
  | Malloc of site
  | Ref of address
  | Deref of address

type cond = rel * value * value

type cmd =
  | Skip
  | Seq of com * com
  | Assign of address * expr
  | Input of var
  | If of cond * com * com
  | While of cond * com

and com = label * cmd

module MemAddress = struct
  type t = address

  let compare a b =
    match (a, b) with
    | Stack x, Stack y -> x - y
    | Heap (site_a, inst_a), Heap (site_b, inst_b) ->
        if site_a == site_b then inst_a - inst_b else site_a - site_b
    (* All stack addrs are before heap locs *)
    | Stack _, Heap _ -> -1
    | Heap _, Stack _ -> 1
end

module Memory = Map.Make (MemAddress)

(* read_mem: var -> mem -> const *)
let read_mem addr mem = Memory.find addr mem

(* write_mem: var -> const -> mem -> mem *)
let write_mem key value memory = Memory.add key value memory

let new_heap_addr site memory =
  let filter_heap addr _value =
    match addr with Heap (_, _) -> true | _ -> false
  in
  (* Just heap addresses from this site *)
  let new_inst = Memory.cardinal (Memory.filter filter_heap memory) in
  Heap (site, new_inst)

(* TODO: add mem arg to print the value as well *)
let print_addr addr =
  match addr with
  | Stack v -> "Stack(" ^ string_of_int v ^ ")"
  | Heap (s, i) -> "Heap(" ^ string_of_int s ^ ", " ^ string_of_int i ^ ")"

let print_value v =
  match v with Int x -> string_of_int x | Address a -> print_addr a

let print_cond (rel, v0, v1) =
  let rel_str =
    match rel with
    | Lt -> "<"
    | Le -> "<="
    | Gt -> ">"
    | Ge -> ">="
    | Eq -> "=="
    | Ne -> "!="
  in
  Printf.printf "%s %s %s" (print_value v0) rel_str (print_value v1)
