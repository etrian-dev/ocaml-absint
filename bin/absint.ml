open Sem

let print_val idx v = Printf.printf "mem[%d] = %d\n" idx v

let _ =
  let init_mem = Array.make 10 0 in
  let newmem = sem_cmd Skip init_mem in
  Printf.printf "Test 1:\n";
  Array.iteri print_val newmem

let _ =
  let init_mem = Array.make 10 0 in
  let newmem = sem_cmd (Assign (1, Const 10)) init_mem in
  Printf.printf "\nTest 2:\n";
  Array.iteri print_val newmem

let _ =
  let init_mem = Array.make 10 0 in
  let newmem = sem_cmd (Seq (Assign(1, Const 5), Assign(2, Bop(Mul, Var(1), Bop(Add, Const 2, Const 1)))) ) init_mem in
  Printf.printf "\nTest 3:\n";
  Array.iteri print_val newmem
