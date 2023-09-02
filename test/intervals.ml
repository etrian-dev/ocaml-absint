(* open Absint_lib.Analyzer.Interval_Analyzer *)
(* open Absint_lib.Intervals *)
(* open Absint_lib.Language *)

(* let mem_init size abs_val =
     Memory.add_seq (Seq.init size (fun n -> (Stack n, abs_val))) Memory.empty

   let mem_sub start endv mem =
     Memory.filter
       (fun k _v -> match k with Stack n -> n >= start && n <= endv | _ -> false)
       mem
*)
(* let pp_bound b = function
   | Inf_Neg -> Fmt.pf b "-oo"
   | Inf_Pos -> Fmt.pf b "+oo"
   | Val v -> Fmt.pf b "%d" v
*)
(* let pp_interval x = function
   | Interval (a, b) ->
       let low_paren = match a with Val _ -> '[' | _ -> '(' in
       let hi_paren = match b with Val _ -> ']' | _ -> ')' in
       Fmt.pf x "%c%a, %a%c" low_paren pp_bound a pp_bound b hi_paren
*)
(* let abs_cmp a b = a = b *)

(* let testable_abs_val = Alcotest.array (Alcotest.testable pp_interval abs_cmp) *)

(*
Analyze the program

x_0 = 1;
while(x_0 <= 10) { x_0 = x_0 + 1; }

pre: T for all vars
post: T for all vars, except x_0 in [0, 10]
*)
(* let test_while1 () =
   Alcotest.(check testable_abs_val)
     "Pre: all Top, Post: all Top except x_0 in [1, 11]"
     [| Interval (Val 1, Val 11); val_top; val_top; val_top; val_top |]
     (let prog =
        Seq
          ( (0, Assign (Stack 0, Const 1)),
            ( 1,
              While
                ( (Le, Int 0, Int 10),
                  (2, Assign (Stack 0, Bop (Add, Var 0, Const 1))) ) ) )
      in
      abs_command prog (mem_init 5 val_top))
*)
(*
  Analyze the program

  x_0 = 1;
  while(x_0 <= 10) { x_0 = x_0 + 1; }
  x_0 = -5

  pre: T for all vars
  post: T for all vars, except x_0 in [-5, -5]
*)
(* let test_while2 () =
   Alcotest.(check testable_abs_val)
     "Pre: all Top, Post: all Top except x_0 <= 0"
     [| Interval (Val (-5), Val (-5)); val_top; val_top; val_top; val_top |]
     (let prog =
        Seq
          ( (0, Assign (Stack 0, Const 1)),
            ( 1,
              Seq
                ( ( 2,
                    While
                      ( (Le, Int 0, Int 10),
                        (3, Assign (Stack 0, Bop (Add, Var 0, Const 1))) ) ),
                  (4, Assign (Stack 0, Const (-5))) ) ) )
      in
      abs_command prog (mem_init 5 val_top))
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
     "Pre: all Top, Post: all Top" (Array.make 5 val_top)
     (let prog =
        Seq
          ( (0, Assign (Stack 0, Const 1)),
            ( 1,
              While
                ( (Gt, Int 0, Int 0),
                  (2, Assign (Stack 0, Bop (Add, Var 1, Const 1))) ) ) )
      in
      abs_command prog (mem_init 5 val_top))
*)
(*
  Analyze the program

  x_0 = 5;
  while ( x_0 <= -1 ) { x_1 = 1; }

  pre: T for all vars
  post: T for all vars except x_0 in [0, +inf]
*)
(* let test_while4 () =
   Alcotest.(check testable_abs_val)
     "Pre: all Top, Post: all top except x_0 in [5, 5]"
     [| Interval (Val 5, Val 5); val_top; val_top; val_top; val_top |]
     (let prog =
        Seq
          ( (0, Assign (Stack 0, Const 5)),
            (1, While ((Le, Int 0, Int (-1)), (2, Assign (Stack 1, Const 1)))) )
      in
      abs_command prog (mem_init 5 val_top))
*)
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
(* let test_if1 () =
   Alcotest.(check testable_abs_val)
     "Pre: all Top, Post: all Top except x_0 >= 0"
     [| Interval (Val 1, Inf_Pos); val_top; val_top; val_top; val_top |]
     (let prog =
        If ((Gt, Int 0, Int 1), (0, Skip), (1, Assign (Stack 0, Const 1)))
      in
      abs_command prog (mem_init 5 val_top))
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
     [| val_top; Interval (Val 10, Val 10); val_top; val_top; val_top |]
     (let prog =
        If
          ( (Gt, Int 0, Int 1),
            (0, Assign (Stack 1, Const 10)),
            ( 1,
              Seq ((1, Assign (Stack 2, Const 10)), (2, Assign (Stack 1, Var 2)))
            ) )
      in
      abs_command prog (mem_init 5 val_top))
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
