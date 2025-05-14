open Type_inference
open Inference
open Expressions

let () =
  infer_and_print @@ Lambda ("x", LiteralInt 1);
  infer_and_print @@ Lambda ("x", Var "x");
  infer_and_print @@ Lambda ("y", Lambda ("z", Var "z"));
  infer_and_print @@ Lambda ("y", Lambda ("z", Var "y"));

  print_endline "\nThe following should be the same semantically: ";
  infer_and_print
  @@ Apply (Lambda ("x", Var "x"), Lambda ("y", Lambda ("z", Var "z")));
  infer_and_print
  @@ Let
       ( "x",
         Lambda ("y", Var "y"),
         Apply (Var "x", Lambda ("y", Lambda ("z", Var "z"))) );

  print_endline
    "\n\
     And now a polymorphic type. x must be polymorphic for this to be \
     well-typed:";
  infer_and_print
  @@ Let
       ( "x",
         Lambda ("y", Var "y"),
         Let
           ( "z",
             Apply (Var "x", LiteralBool true),
             Let ("t", Apply (Var "x", LiteralInt 1), Var "x") ) )
