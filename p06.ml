open OUnit2

(* Find out whether a list is a palindrome. *)

let reverse l =
  let arr = Array.of_list l in
  let n = Array.length arr in
  List.init n (fun i -> arr.(n-1-i))

let is_palindrome l = (reverse l = l)

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae true (is_palindrome ["x"; "a"; "m"; "a"; "x"]);
  "test2" >:: ae true (not @@ is_palindrome ["a"; "b"])
]

let () =
  run_test_tt_main ("tests" >::: tests)
