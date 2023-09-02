open Language
open Domain
open Analyzer

type mode = Definitely | Maybe
type abs_addr = Addr of var | Abs_Addr of var
type pto = Pto of abs_addr list | Unknown

(* var definitely points to nothing *)
let pto_bot = Pto []

(* var points to anything *)
let pto_top = Unknown

let cmp_ptos x y =
  match (x, y) with
  | Addr v, Addr w -> v - w
  | Addr _, Abs_Addr _ -> -1
  | Abs_Addr _, Addr _ -> 1
  | Abs_Addr v, Abs_Addr w -> v - w

let pto_incl pto1 pto2 =
  match (pto1, pto2) with
  | Unknown, Unknown -> true
  | Unknown, Pto _ | Pto _, Unknown -> false
  (* For each Pto in p1, there exists such element in p2, that is,
     the subset of possible (or definite) pointees of p1 is a subset of p2
     e.g. pto_incl Pto{x} Pto{x,y} -> true, while the reverse is false *)
  | Pto p1, Pto p2 ->
      List.for_all (fun x -> List.exists (fun y -> cmp_ptos x y = 0) p2) p1

let is_definitely pto =
  match pto with
  | Pto [] | Pto (_ :: []) -> true
  | Pto (_ :: _) | Unknown -> false

let add_pto abs_val addr =
  let item =
    match addr with Stack var -> Addr var | Heap (site, _) -> Abs_Addr site
  in
  match abs_val with
  | Unknown -> Pto [ item ]
  | Pto ptos ->
      let new_ptos = List.sort_uniq cmp_ptos (item :: ptos) in
      Pto new_ptos

let intersect_pto pto1 pto2 =
  match (pto1, pto2) with
  | Pto l1, Pto l2 ->
      Pto (List.filter_map (fun x -> List.find_opt (fun y -> y = x) l2) l1)
  | _ -> pto_top

let join_ptos pto1 pto2 =
  match (pto1, pto2) with
  | Unknown, Unknown -> Unknown
  | Unknown, Pto _ -> pto2
  | Pto _, Unknown -> pto1
  | Pto l1, Pto l2 -> Pto (List.merge cmp_ptos l1 l2)

let val_cnst value =
  match value with
  | Int _ -> pto_bot
  | Address (Stack var) -> Pto [ Addr var ]
  (* All allocs from a malloc site point to the same abstract address, so
     that the abstract address space size is finite *)
  | Address (Heap (site, _)) -> Pto [ Abs_Addr site ]

module Points_To (Dom : Domain) = struct
  module Abstract_Analyzer = Analyzer (Dom)

  let abs_cond (rel, v0, v1) aenv =
    match (rel, v0, v1) with
    | Eq, Address a, Address b -> (
        let pto_a = Memory.find a aenv in
        let pto_b = Memory.find b aenv in
        if is_definitely pto_b then write_mem a pto_b aenv
        else
          let common_pto = intersect_pto pto_a pto_b in
          match (common_pto, pto_a, pto_b) with
          (* There are common pointees (update both pointee lists)*)
          | Pto com, _, _ -> write_mem b (Pto com) (write_mem a (Pto com) aenv)
          (* No common pointees, or *)
          | Unknown, Unknown, Unknown -> write_mem a pto_top aenv
          (* No common ptos -> branch can't be reached *)
          | Unknown, Pto _, Pto _ -> write_mem a pto_bot aenv
          (* Narrow down to the most specific pto *)
          | Unknown, Pto alist, Unknown -> write_mem b (Pto alist) aenv
          | Unknown, Unknown, Pto blist -> write_mem a (Pto blist) aenv)
    | Ne, Address a, Address b -> (
        let pto_a = Memory.find a aenv in
        let pto_b = Memory.find b aenv in
        let common_pto = intersect_pto pto_a pto_b in
        match (common_pto, pto_a, pto_b) with
        (* No intersection: they are definitely different*)
        | Unknown, Pto _, Pto _ -> aenv
        | Unknown, Pto alist, Unknown -> write_mem b (Pto alist) aenv
        | Unknown, Unknown, Pto blist -> write_mem a (Pto blist) aenv
        (* There's an intersection: branch is reachable iff their actual value is not equal.
           An approximation is that the branch is unreachable (hence the pto_bot for both)*)
        | Pto _, _, _ -> write_mem b pto_bot (write_mem a pto_bot aenv)
        | Unknown, Unknown, Unknown -> aenv)
    | _ -> aenv

  let rec abs_command cmd pre =
    match cmd with
    | Seq ((_, c1), (_, c2)) -> abs_command c2 (abs_command c1 pre)
    | Assign (addr, expr) -> (
        match expr with
        | Ref ptee_addr ->
            let x = add_pto (read_mem addr pre) ptee_addr in
            write_mem addr x pre
        | _ -> pre)
    | If (cond, (_, then_cmd), (_, else_cmd)) ->
        let then_aenv = abs_command then_cmd (abs_cond cond pre) in
        let else_aenv =
          abs_command else_cmd
            (abs_cond (Abstract_Analyzer.negate_cond cond) pre)
        in
        (* Join the pto sets for each variable in the memory *)
        Memory.union
          (fun _k pto1 pto2 -> Some (join_ptos pto1 pto2))
          then_aenv else_aenv
    | _ -> pre
end
