open Sexplib.Conv

module Core_kernel = struct
  module Std = struct

    exception E of string * Sexplib.Sexp.t with sexp
    let failwiths msg t sexp_of_t =
      raise (E (msg, sexp_of_t t))

    let () =
      Printexc.register_printer (fun exc ->
        match Sexplib.Conv.sexp_of_exn_opt exc with
        | None -> None
        | Some sexp ->
          Some (Sexplib.Sexp.to_string_hum ~indent:2 sexp))

    module Source_code_position = struct
      type t = Lexing.position

        let sexp_of_t_hum t =
          <:sexp_of< [ `file of string ] * [ `line of int ] >>
            (`file t.Lexing.pos_fname, `line t.Lexing.pos_lnum)
    end

  end
end


TEST_UNIT = <:test_eq< int >> 1 1
TEST =
  try <:test_eq< int * int >> ~here:[_here_] (5, 5) (5, 6); false
  with _ -> true

TEST =
  try <:test_pred< float >> ((=) 3.) 5.; false
  with _ -> true

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
