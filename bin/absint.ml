open Language
open Analyzer

(*
  Analyze the program

  x_0 = 1;
  while(x_0 <= 10) { x_0 = x_0 + 1; }

  pre: T for all vars
  post: T for all vars, except x_0 is Apos
*)
let _ =
  let init_mem = Array.make 5 Signs.Atop in
  let prog = Seq(
    (0, Assign(0, Const 1)),
    (1, Seq(
      (2, While((Rle, 0, 10), (3, Assign(0, Bop(Add, Var(1), Const 1))))),
      (4, Assign(0, Const (-5)))
    ))
   ) in
  Printf.printf "\nBefore the analysis:\n";
  Array.iteri print_abs_val init_mem;
  let analysis = abs_command prog init_mem in
  Printf.printf "\nAfter the analysis:\n";
  Array.iteri print_abs_val analysis

(*
  Analyze the program

  x_0 = 1;
  while(x_0 <= 10) { x_0 = x_0 + 1; }
  x_0 = -5

  pre: T for all vars
  post: T for all vars, except x_0 is Aneg
*)
let _ =
  let init_mem = Array.make 5 Signs.Atop in
  let prog = Seq(
    (0, Assign(0, Const 1)),
    (1, Seq(
      (2, While((Rle, 0, 10), (3, Assign(0, Bop(Add, Var(1), Const 1))))),
      (4, Assign(0, Const (-5)))
    ))
   ) in
  Printf.printf "\nBefore the analysis:\n";
  Array.iteri print_abs_val init_mem;
  let analysis = abs_command prog init_mem in
  Printf.printf "\nAfter the analysis:\n";
  Array.iteri print_abs_val analysis

(*
  Analyze the program

  x_0 = 1;
  while(x_0 >= 0) { x_0 = x_0 + 1 }

  pre: T for all vars
  post: T for all vars
*)
let _ =
  let init_mem = Array.make 5 Signs.Atop in
  let prog = Seq(
    (0, Assign(0, Const 1)),
    (1, While((Rgt, 0, 0), (2, Assign(0, Bop(Add, Var(1), Const 1)))))
  ) in
  Printf.printf "\nBefore the analysis:\n";
  Array.iteri print_abs_val init_mem;
  let analysis = abs_command prog init_mem in
  Printf.printf "\nAfter the analysis:\n";
  Array.iteri print_abs_val analysis

(*
  Analyze the program

  if( x_0 > 1 ) {
    ;
  } else {
    x_0 = 1;
  }

  pre: T for all vars
  post: T for all vars, except x_0 = Apos
*)
let _ =
  let init_mem = Array.make 5 Signs.Atop in
  let prog = If(
    (Rgt, 0, 1),
    (0, Skip),
    (1, Assign(0, Const 1))
  ) in
  Printf.printf "\nBefore the analysis:\n";
  Array.iteri print_abs_val init_mem;
  let analysis = abs_command prog init_mem in
  Printf.printf "\nAfter the analysis:\n";
  Array.iteri print_abs_val analysis

(*
Analyze the program

if( x_0 > 1 ) {
  x_1 = 10;
} else {
  x_2 = 10;
  x_1 = x_2;
}

pre: T for all vars
post: T for all vars, except x_1 = Apos and x_2 = Apos
*)
let _ =
let init_mem = Array.make 5 Signs.Atop in
let prog = If(
  (Rgt, 0, 1),
  (0, Assign(0, Const 10)),
  (1, Seq(
    (1, Assign(2, Const 10)),
    (2, Assign(1, Var 2))
  ))
) in
Printf.printf "\nBefore the analysis:\n";
Array.iteri print_abs_val init_mem;
let analysis = abs_command prog init_mem in
Printf.printf "\nAfter the analysis:\n";
assert (read_mem 1 analysis = Apos); (* This fails *)
assert (read_mem 2 analysis = Apos);
Array.iteri print_abs_val analysis
