open OUnit2

(* Reverse a list. *)

let reverse l =
  let arr = Array.of_list l in
  let n = Array.length arr in
  List.init n (fun i -> arr.(n-1-i))

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae ["c"; "b"; "a"] (reverse ["a"; "b"; "c"])
]

let () =
  run_test_tt_main ("tests" >::: tests)
