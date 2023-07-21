open Absint_lib.Analyzer.Interval_Analyzer
open Absint_lib.Intervals
open Absint_lib.Language

let mem_init n value = Array.make n value

let pp_bound b = function
  | Inf_Neg -> Fmt.pf b "-oo"
  | Inf_Pos -> Fmt.pf b "+oo"
  | Val v -> Fmt.pf b "%d" v

let pp_interval x = function
  | Interval (a, b) ->
      let low_paren = match a with Val _ -> '[' | _ -> '(' in
      let hi_paren = match b with Val _ -> ']' | _ -> ')' in
      Fmt.pf x "%c%a, %a%c" low_paren pp_bound a pp_bound b hi_paren

let abs_cmp a b = a = b
let testable_abs_val = Alcotest.array (Alcotest.testable pp_interval abs_cmp)

(*
Analyze the program

x_0 = 1;
while(x_0 <= 10) { x_0 = x_0 + 1; }

pre: T for all vars
post: T for all vars, except x_0 in [0, 10]
*)
let test_while1 () =
  Alcotest.(check testable_abs_val)
    "Pre: all Top, Post: all Top except x_0 in [1, 11]"
    [| Interval (Val 1, Val 11); val_top; val_top; val_top; val_top |]
    (let prog =
       Seq
         ( (0, Assign (0, Const 1)),
           (1, While ((Rle, 0, 10), (2, Assign (0, Bop (Add, Var 0, Const 1)))))
         )
     in
     abs_command prog (mem_init 5 val_top))

(*
  Analyze the program

  x_0 = 1;
  while(x_0 <= 10) { x_0 = x_0 + 1; }
  x_0 = -5

  pre: T for all vars
  post: T for all vars, except x_0 in [-5, -5]
*)
let test_while2 () =
  Alcotest.(check testable_abs_val)
    "Pre: all Top, Post: all Top except x_0 <= 0"
    [| Interval (Val (-5), Val (-5)); val_top; val_top; val_top; val_top |]
    (let prog =
       Seq
         ( (0, Assign (0, Const 1)),
           ( 1,
             Seq
               ( ( 2,
                   While
                     ((Rle, 0, 10), (3, Assign (0, Bop (Add, Var 0, Const 1))))
                 ),
                 (4, Assign (0, Const (-5))) ) ) )
     in
     abs_command prog (mem_init 5 val_top))

(*
  Analyze the program

  x_0 = 1;
  while(x_0 >= 0) { x_0 = x_0 + 1 }

  pre: T for all vars
  post: T for all vars
*)
let test_while3 () =
  Alcotest.(check testable_abs_val)
    "Pre: all Top, Post: all Top" (Array.make 5 val_top)
    (let prog =
       Seq
         ( (0, Assign (0, Const 1)),
           (1, While ((Rgt, 0, 0), (2, Assign (0, Bop (Add, Var 1, Const 1)))))
         )
     in
     abs_command prog (mem_init 5 val_top))

(*
  Analyze the program

  x_0 = 5;
  while ( x_0 <= -1 ) { x_1 = 1; }

  pre: T for all vars
  post: T for all vars except x_0 in [0, +inf]
*)
let test_while4 () =
  Alcotest.(check testable_abs_val)
    "Pre: all Top, Post: all top except x_0 in [5, 5]"
    [| Interval (Val 5, Val 5); val_top; val_top; val_top; val_top |]
    (let prog =
       Seq
         ( (0, Assign (0, Const 5)),
           (1, While ((Rle, 0, -1), (2, Assign (1, Const 1)))) )
     in
     abs_command prog (mem_init 5 val_top))

(*
  Analyze the program

  if( x_0 > 1 ) {
    ;
  } else {
    x_0 = 1;
  }

  pre: T for all vars
  post: T for all vars, except x_0 in [1, +inf]
*)
let test_if1 () =
  Alcotest.(check testable_abs_val)
    "Pre: all Top, Post: all Top except x_0 >= 0"
    [| Interval (Val 1, Inf_Pos); val_top; val_top; val_top; val_top |]
    (let prog = If ((Rgt, 0, 1), (0, Skip), (1, Assign (0, Const 1))) in
     abs_command prog (mem_init 5 val_top))

(*
Analyze the program

if( x_0 > 1 ) {
  x_1 = 10;
} else {
  x_2 = 10;
  x_1 = x_2;
}

pre: T for all vars
post: T for all vars, except x_1 = Apos
*)
let test_if2 () =
  Alcotest.(check testable_abs_val)
    "Pre: all Top, Post: all Top except x_0 >= 0"
    [| val_top; Interval (Val 10, Val 10); val_top; val_top; val_top |]
    (let prog =
       If
         ( (Rgt, 0, 1),
           (0, Assign (1, Const 10)),
           (1, Seq ((1, Assign (2, Const 10)), (2, Assign (1, Var 2)))) )
     in
     abs_command prog (mem_init 5 val_top))

let while_tests =
  ( "While tests",
    [
      Alcotest.test_case "Infer Apos" `Quick test_while1;
      Alcotest.test_case "Infer Aneg" `Quick test_while2;
      Alcotest.test_case "Keep Atop" `Quick test_while3;
      Alcotest.test_case "Infer Apos, do not infer Aneg" `Quick test_while4;
    ] )

let if_tests =
  ( "If tests",
    [
      Alcotest.test_case "Infer Apos" `Quick test_if1;
      Alcotest.test_case "Infer Apos" `Quick test_if2;
    ] )

let () = Alcotest.run "Analyzer test suite" [ while_tests; if_tests ]
