open Sexplib
open Sexplib.Conv

module Sexp = struct
  include Sexp
  let compare = Pervasives.compare
end

module Core_kernel = struct
  module Std = struct

    exception E of string * Sexp.t with sexp
    let failwiths msg t sexp_of_t =
      raise (E (msg, sexp_of_t t))

    let () =
      Printexc.register_printer (fun exc ->
        match sexp_of_exn_opt exc with
        | None -> None
        | Some sexp ->
          Some (Sexp.to_string_hum ~indent:2 sexp))

    module Source_code_position = struct
      type t = Lexing.position

        let sexp_of_t_hum t =
          <:sexp_of< [ `file of string ] * [ `line of int ] >>
            (`file t.Lexing.pos_fname, `line t.Lexing.pos_lnum)
    end

  end
end

let re_line = Str.regexp "line [0-9]*"
let re_characters = Str.regexp "characters [0-9-]*"
let re_file = Str.regexp "(file [^)]*)"
let hide_position_details str =
  let str = Str.global_replace re_line "line X" str in
  let str = Str.global_replace re_characters "characters X-X" str in
  Str.global_replace re_file "(file X)" str

let test_exn exn str =
  let sexp_str = Sexp.of_string str in
  let sexp_exn =
    match sexp_of_exn_opt exn with
    | None -> assert false
    | Some sexp ->
      Sexp.of_string (hide_position_details (Sexp.to_string sexp))
  in
  <:test_eq< Sexp.t >> sexp_exn sexp_str

TEST_UNIT = <:test_eq< int >> 1 1
TEST =
  try <:test_eq< int * int >> ~here:[_here_] ~message:"int tuple" (5, 5) (5, 6); false
  with e ->
    test_exn e "(pa_test_test.ml.Core_kernel.Std.E \"int tuple: comparison failed\"
                 ((5 5) vs (5 6)
                  (Loc \"File \\\"pa_test_test.ml\\\", line X, characters X-X\")
                 (Stack (((file X) (line X))))))";
    true

TEST_UNIT = <:test_result< int >> (1 + 2) ~message:"size" ~expected:3
TEST =
  try <:test_result< int * int >> ~here:[_here_] (5, 5) ~expected:(5, 6); false
  with e ->
    test_exn e "(pa_test_test.ml.Core_kernel.Std.E \"got unexpected result\"
              ((got (5 5)) (expected (5 6))
                (Loc \"File \\\"pa_test_test.ml\\\", line X, characters X-X\")
                (Stack (((file X) (line X))))))";
    true

TEST =
  try <:test_pred< float >> ~message:"price" ((=) 3.) 5.; false
  with e ->
    test_exn e "(pa_test_test.ml.Core_kernel.Std.E \"price: predicate failed\"
              ((Value 5) (Loc \"File \\\"pa_test_test.ml\\\", line X, characters X-X\")))";
    true

TEST_UNIT = <:test_eq< int >> ~equal:(fun x y -> x mod 2 = y mod 2) 4 6

(* An example where the list of positions that <:test_eq< ... >> takes comes in handy,
   because the position of <:test_eq< ... >> itself is not very informative. *)
let test_is_zero ~here x = <:test_eq< int >> 0 x ~here:(_here_ :: here)

let test_odds n ~here =
  for i = 0 to n do
    let odd = 2 * i + 1 in
    test_is_zero ~here:(_here_ :: here) (odd - odd)
  done

let test_evens n ~here =
  for i = 0 to n do
    let even = 2 * i in
    test_is_zero ~here:(_here_ :: here) (even - even)
  done

let test_all n =
  test_odds n ~here:[_here_];
  test_evens n ~here:[_here_]

TEST_UNIT = test_all 10
