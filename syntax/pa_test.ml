open Camlp4.PreCast

let compare =
  try Syntax.Quotation.find "compare" Syntax.Quotation.DynAst.expr_tag
  with Not_found -> failwith "Using pa_test requires pa_compare to be loaded"
let sexp_of =
  try Syntax.Quotation.find "sexp_of" Syntax.Quotation.DynAst.expr_tag
  with Not_found -> failwith "Using pa_test requires pa_sexp_conv to be loaded"

let () =
  Syntax.Quotation.add "test_pred"
    Syntax.Quotation.DynAst.expr_tag
    (fun loc loc_name_opt cnt_str ->
      Pa_type_conv.set_conv_path_if_not_set loc;
      let pos = Pa_here.ast_of_loc loc in
      let sexpifier = sexp_of loc loc_name_opt cnt_str in
      <:expr@loc< fun ?(here = []) ?message predicate t ->
        let pos = $pos$ in
        let sexpifier = $sexpifier$ in
        Pa_test_lib.Runtime.test_pred
          ~pos ~sexpifier ~here ?message predicate t
        >>
    )


let () =
  Syntax.Quotation.add "test_eq"
    Syntax.Quotation.DynAst.expr_tag
    (fun loc loc_name_opt cnt_str ->
      Pa_type_conv.set_conv_path_if_not_set loc;
      let pos = Pa_here.ast_of_loc loc in
      let comparator = compare loc loc_name_opt cnt_str in
      let sexpifier = sexp_of loc loc_name_opt cnt_str in
      <:expr@loc< fun ?(here = []) ?message ?equal t1 t2 ->
        let pos = $pos$ in
        let sexpifier = $sexpifier$ in
        let comparator = $comparator$ in
        Pa_test_lib.Runtime.test_eq
          ~pos ~sexpifier ~comparator ~here ?message ?equal t1 t2
        >>
    )

let () =
  Syntax.Quotation.add "test_result"
    Syntax.Quotation.DynAst.expr_tag
    (fun loc loc_name_opt cnt_str ->
      Pa_type_conv.set_conv_path_if_not_set loc;
      let pos = Pa_here.ast_of_loc loc in
      let comparator = compare loc loc_name_opt cnt_str in
      let sexpifier = sexp_of loc loc_name_opt cnt_str in
      <:expr@loc< fun ?(here = []) ?message ?equal ~expect got ->
        let pos = $pos$ in
        let sexpifier = $sexpifier$ in
        let comparator = $comparator$ in
        Pa_test_lib.Runtime.test_result
          ~pos ~sexpifier ~comparator ~here ?message ?equal ~expect ~got
        >>
    )
