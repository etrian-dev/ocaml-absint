open Language

let dump_mem =
  Language.dump_mem Language.string_of_addr Language.string_of_value

let extract_const value =
  match value with
  | Int c1 -> c1
  | _ -> failwith "Cannot extract a non-constant value"

(** Semantic of a binary expression
    @param op The binary operator. See type [Language.bop]
    @param c1 Left operand, constant
    @param c2 Right operand, constant
    @return The result of [c1 op c2] *)
let sem_bop op v1 v2 =
  let c1 = extract_const v1 in
  let c2 = extract_const v2 in
  match op with
  | Add -> Int (c1 + c2)
  | Sub -> Int (c1 - c2)
  | Mul -> Int (c1 * c2)
  | Div -> Int (c1 / c2)

let sem_uop op v1 = match op with Minus -> Int (-extract_const v1)

(* Semantic of an expression: a function expr -> mem -> const *)
let rec sem_expr expr mem =
  match expr with
  | Const c -> (Int c, mem)
  | Var v -> (read_mem (Stack v) mem, mem)
  | Uop (op, exp) ->
      let v, _ = sem_expr exp mem in
      (sem_uop op v, mem)
  | Bop (op, lhs, rhs) ->
      let v1, _ = sem_expr lhs mem in
      let v2, _ = sem_expr rhs mem in
      (sem_bop op v1 v2, mem)
  | Malloc site ->
      let heap_addr = new_heap_addr site mem in
      (Address heap_addr, write_mem heap_addr (Int 0) mem)
  | Ref addr -> (Address addr, mem)
  | Deref addr -> (read_mem addr mem, mem)

(* Semantic of a relation: a function rel -> expr -> expr -> bool *)
let sem_rel rel v0 v1 =
  match rel with
  | Lt -> v0 < v1
  | Le -> v0 <= v1
  | Gt -> v0 > v1
  | Ge -> v0 >= v1
  | Eq -> v0 == v1
  | Ne -> v0 != v1

(* Semantic of a condition: a function cond -> mem -> bool *)
let sem_cond (rel, v0, v1) = sem_rel rel v0 v1

(* Semantic of a command: a function cmd -> mem -> mem *)
let rec sem_cmd cmd mem =
  match cmd with
  | Skip -> mem
  | Seq ((_, c0), (_, c1)) -> sem_cmd c1 (sem_cmd c0 mem)
  | Assign (addr, expr) ->
      let value, newmem = sem_expr expr mem in
      write_mem addr value newmem
  | Input _ -> mem (* Scanf.scanf... *)
  | If (cond, (_, cmd1), (_, cmd2)) ->
      if sem_cond cond then sem_cmd cmd1 mem else sem_cmd cmd2 mem
  | While (cond, (_, com)) ->
      if sem_cond cond then sem_cmd cmd (sem_cmd com mem) else mem

let print_val idx v = Printf.printf "mem[%d] = %d\n" idx v

let init_mem =
  Memory.add_seq (Seq.init 10 (fun n -> (Stack n, Int 0))) Memory.empty

let mem_sub start endv mem =
  Memory.filter
    (fun k _v -> match k with Stack n -> n >= start && n <= endv | _ -> false)
    mem

let%test "skip cmd" =
  let newmem = sem_cmd Skip init_mem in
  Memory.equal (fun v1 v2 -> v1 = v2) init_mem newmem

let%test "assign cmd" =
  let newmem = sem_cmd (Assign (Stack 0, Const 10)) init_mem in
  Memory.find (Stack 0) newmem = Int 10
  && Memory.for_all
       (fun _k v -> if Int 0 = v then true else false)
       (mem_sub 1 (Memory.cardinal newmem) newmem)

let%test "seq cmd" =
  let newmem =
    sem_cmd
      (Seq
         ( (0, Assign (Stack 0, Const 5)),
           (1, Assign (Stack 1, Bop (Mul, Var 0, Bop (Add, Const 2, Const 1))))
         ))
      init_mem
  in
  Memory.find (Stack 0) newmem = Int 5
  && Memory.find (Stack 1) newmem = Int 15
  && Memory.equal
       (fun v1 v2 -> v1 = v2)
       (mem_sub 2 (Memory.cardinal init_mem) init_mem)
       (mem_sub 2 (Memory.cardinal newmem) newmem)
