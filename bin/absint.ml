open Sem

let print_val idx v = Printf.printf "mem[%d] = %d\n" idx v

let _ =
  let init_mem = Array.make 10 0 in
  let newmem = sem_cmd (Assign (1, Const 10)) init_mem in
  Array.iteri print_val newmem
