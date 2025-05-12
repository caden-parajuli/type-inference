open Type_inference
open Type_inference.Expressions

let () = 
    (* print_endline @@ Types.string_of_type @@ *)
    match Inference.infer @@ Lambda ("x", (Var "x")) with
    | Some (sub, t) -> 
        let () = List.fold_left
            (fun _ -> function | (v, t) -> print_endline @@ (Types.string_of_type t) ^ " / " ^ v)
            ()
            sub in
        print_endline @@ Types.string_of_type t
    | None -> print_endline "Not well-typed"

      
