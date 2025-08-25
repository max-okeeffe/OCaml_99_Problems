open OUnit2

(* Drop every N'th element from a list. *)

let drop l n =
  let rec aux l i out =
    match l with
    | [] -> out
    | h :: t ->
      match i with
      | i' when i' mod n = 0 -> aux t (i+1) out
      | _ -> aux t (i+1) (out @ [h]) in
  aux l 1 []

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae ["a"; "b"; "d"; "e"; "g"; "h"; "j"] (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3)
]

let () =
  run_test_tt_main ("tests" >::: tests)
