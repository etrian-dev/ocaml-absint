type label = int
type const = label
type var = const
type bop = Add | Sub | Mul | Div
type uop = Minus
type rel = Lt | Le | Gt | Ge | Eq | Ne
type site = var
type heap_address = site * site
type address = Stack of site | Heap of heap_address
type value = Int of site | Address of address

type expr =
  | Const of site
  | Var of site
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
  | Input of site
  | If of cond * com * com
  | While of cond * com

and com = site * cmd

module MemAddress : sig
  type t = address

  val compare : address -> address -> site
end

module Memory : sig
  type key = address
  type 'a t = 'a Map.Make(MemAddress).t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t

  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val compare : ('a -> 'a -> label) -> 'a t -> 'a t -> label
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> label
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val min_binding_opt : 'a t -> (key * 'a) option
  val max_binding : 'a t -> key * 'a
  val max_binding_opt : 'a t -> (key * 'a) option
  val choose : 'a t -> key * 'a
  val choose_opt : 'a t -> (key * 'a) option
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val find_first : (key -> bool) -> 'a t -> key * 'a
  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val find_last : (key -> bool) -> 'a t -> key * 'a
  val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_rev_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
  val of_seq : (key * 'a) Seq.t -> 'a t
end

val read_mem : address -> 'a Memory.t -> 'a
val write_mem : Memory.key -> 'a -> 'a Memory.t -> 'a Memory.t
val new_heap_addr : site -> 'a Memory.t -> address
val string_of_addr : address -> string
val string_of_value : value -> string
val print_addr : address -> unit
val print_value : value -> unit
val print_cond : rel * value * value -> unit
val dump_mem : (address -> string) -> ('a -> string) -> 'a Memory.t -> unit
