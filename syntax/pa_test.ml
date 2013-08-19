open Camlp4.PreCast

let compare =
  try Syntax.Quotation.find "compare" Syntax.Quotation.DynAst.expr_tag
  with Not_found -> failwith "Using pa_test requires pa_compare to be loaded"
let sexp_of =
  try Syntax.Quotation.find "sexp_of" Syntax.Quotation.DynAst.expr_tag
  with Not_found -> failwith "Using pa_test requires pa_sexp_conv to be loaded"

let sexp_list_expr_of_loc loc =
  <:expr@loc<
    [ Sexplib.Sexp.List [
        Sexplib.Sexp.Atom "Loc"; Sexplib.Sexp.Atom $str:Loc.to_string loc$;
      ] ::
      match here with
        [ [] -> []
        | _ ->
          [ Sexplib.Sexp.List [
            Sexplib.Sexp.Atom "Stack";
            Sexplib.Conv.sexp_of_list Core_kernel.Std.Source_code_position.to_sexp_hum here;
          ] ] ] ] >>

let () =
  Syntax.Quotation.add "test_pred"
    Syntax.Quotation.DynAst.expr_tag
    (fun loc loc_name_opt cnt_str ->
      Pa_type_conv.set_conv_path_if_not_set loc;
      let sexpifier = sexp_of loc loc_name_opt cnt_str in
      <:expr@loc< fun ?(here = []) predicate t ->
       if not (predicate t) then begin
         Core_kernel.Std.failwiths "predicate failed"
           (Sexplib.Sexp.List [
             Sexplib.Sexp.List [
               Sexplib.Sexp.Atom "Value"; $sexpifier$ t;
             ] :: $sexp_list_expr_of_loc loc$ ]) (fun x -> x)
       end else ()
      >>
    )

let () =
  Syntax.Quotation.add "test_eq"
    Syntax.Quotation.DynAst.expr_tag
    (fun loc loc_name_opt cnt_str ->
      Pa_type_conv.set_conv_path_if_not_set loc;
      let comparator = compare loc loc_name_opt cnt_str in
      let sexpifier = sexp_of loc loc_name_opt cnt_str in
      <:expr@loc< fun ?(here = []) ?equal t1 t2 ->
       let sexpifier = $sexpifier$ in
       let pass =
         match equal with
         [ None ->
           match $comparator$ t1 t2 with
           [ 0 -> True
           | _ -> False ]
         | Some f -> f t1 t2 ] in
       if not pass then begin
         Core_kernel.Std.failwiths "comparison failed"
           (Sexplib.Sexp.List [
             sexpifier t1;
             Sexplib.Sexp.Atom "vs";
             sexpifier t2
             :: $sexp_list_expr_of_loc loc$ ]) (fun x -> x)
       end else ()
      >>
    )
