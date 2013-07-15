(* pa_test defines two quotation expanders, which can be used to reduce boilerplate
   in testing code:

   - <:test_eq< type >> that expands to a function of type
     [?(f = (fun x y -> <:compare< type >> x y = 0)) -> ?here:Lexing.position list -> type -> type -> unit]
     that throws a nice exception if the values are not equal according to [f].

   - <:test_pred< type >> that expands to a function of type
     [?here:Lexing.position list -> (type -> bool) -> type -> unit]
     that throws a nice exception if the predicate fails on the value.

   See the docs repo for more info or ../test for examples.

   For users outside of jane street, please note that the generated code calls Core_kernel
   and sexplib.
*)
