open OUnit2

(* Duplicate the elements of a list. *)

let duplicate l =
  let rec aux l out =
    match l with
    | [] -> out
    | h :: t -> aux t (out @ [h;h]) in
  aux l []

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] (duplicate ["a"; "b"; "c"; "c"; "d"])
]

let () =
  run_test_tt_main ("tests" >::: tests)
