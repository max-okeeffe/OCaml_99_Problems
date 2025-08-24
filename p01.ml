open OUnit2

(* Write a function last : 'a list -> 'a option that returns the last element of a list *)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae (Some "d") (last ["a"; "b"; "c"; "d"]);
  "test2" >:: ae None (last [])
]

let () =
  run_test_tt_main ("tests" >::: tests)
