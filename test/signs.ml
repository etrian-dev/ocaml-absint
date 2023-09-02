(* open Absint_lib.Analyzer.Signs_Analyzer
   open Absint_lib.Signs
   open Absint_lib.Language

   let pp a = function
     | Atop -> Fmt.pf a "T"
     | Abot -> Fmt.pf a "⊥"
     | Apos -> Fmt.pf a ">= 0"
     | Aneg -> Fmt.pf a "<= 0"

   let abs_cmp a b = a = b *)
(* let testable_abs_val = Alcotest.array (Alcotest.testable pp abs_cmp) *)

(* let mem_init size abs_val =
     Memory.add_seq (Seq.init size (fun n -> (Stack n, abs_val))) Memory.empty

   let mem_sub start endv mem =
     Memory.filter
       (fun k _v -> match k with Stack n -> n >= start && n <= endv | _ -> false)
       mem
*)
(*
Analyze the program

x_0 = 1;
while(x_0 <= 10) { x_0 = x_0 + 1; }

pre: T for all vars
post: T for all vars, except x_0 is Apos
*)
(* let test_while1 () =
   Alcotest.(check testable_abs_val)
     "Pre: all Top, Post: all Top except x_0 >= 0"
     [| Apos; Atop; Atop; Atop; Atop |]
     (let prog =
        Seq
          ( (0, Assign (Stack 0, Const 1)),
            ( 1,
              While
                ( (Le, Int 0, Int 10),
                  (2, Assign (Stack 0, Bop (Add, Var 0, Const 1))) ) ) )
      in
      abs_command prog (mem_init 5 Atop))
*)
(*
  Analyze the program

  x_0 = 1;
  while(x_0 <= 10) { x_0 = x_0 + 1; }
  x_0 = -5

  pre: T for all vars
  post: T for all vars, except x_0 is Aneg
*)
(* let test_while2 () =
   Alcotest.(check testable_abs_val)
     "Pre: all Top, Post: all Top except x_0 <= 0"
     [| Aneg; Atop; Atop; Atop; Atop |]
     (let prog =
        Seq
          ( (0, Assign (0, Const 1)),
            ( 1,
              Seq
                ( ( 2,
                    While
                      ((Le, 0, 10), (3, Assign (0, Bop (Add, Var 0, Const 1))))
                  ),
                  (4, Assign (0, Const (-5))) ) ) )
      in
      abs_command prog (mem_init 5 Atop))
*)
(*
  Analyze the program

  x_0 = 1;
  while(x_0 >= 0) { x_0 = x_0 + 1 }

  pre: T for all vars
  post: T for all vars
*)
(* let test_while3 () =
   Alcotest.(check testable_abs_val)
     "Pre: all Top, Post: all Top"
     [| Atop; Atop; Atop; Atop; Atop |]
     (let prog =
        Seq
          ( (0, Assign (0, Const 1)),
            (1, While ((Gt, 0, 0), (2, Assign (0, Bop (Add, Var 1, Const 1)))))
          )
      in
      abs_command prog (mem_init 5 Atop))
*)
(*
  Analyze the program

  x_0 = 5;
  while ( x_0 <= -1 ) { x_1 = 1; }

  pre: T for all vars
  post: T for all vars except x_0 >= 0
*)
(* let test_while4 () =
   Alcotest.(check testable_abs_val)
     "Pre: all Top, Post: all top except x_0 >= 0"
     [| Apos; Atop; Atop; Atop; Atop |]
     (let prog =
        Seq
          ( (0, Assign (0, Const 5)),
            (1, While ((Le, 0, -1), (2, Assign (1, Const 1)))) )
      in
      abs_command prog (mem_init 5 Atop))
*)
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
(* let test_if1 () =
   Alcotest.(check testable_abs_val)
     "Pre: all Top, Post: all Top except x_0 >= 0"
     [| Apos; Atop; Atop; Atop; Atop |]
     (let prog = If ((Gt, 0, 1), (0, Skip), (1, Assign (0, Const 1))) in
      abs_command prog (mem_init 5 Atop))
*)
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
(* let test_if2 () =
   Alcotest.(check testable_abs_val)
     "Pre: all Top, Post: all Top except x_0 >= 0"
     [| Atop; Apos; Atop; Atop; Atop |]
     (let prog =
        If
          ( (Gt, 0, 1),
            (0, Assign (1, Const 10)),
            (1, Seq ((1, Assign (2, Const 10)), (2, Assign (1, Var 2)))) )
      in
      abs_command prog (mem_init 5 Atop))
*)
(* let while_tests =
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
*)
