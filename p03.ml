open OUnit2

(* Find the N'th element of a list. *)

let rec at i l =
  match i, l with
  | _, [] -> None
  | 0, h :: _ -> Some h
  | i', _ :: t -> at (i'-1) t

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae (Some "c") (at 2 ["a"; "b"; "c"; "d"; "e"]);
  "test2" >:: ae None (at 2 ["a"])
]

let () =
  run_test_tt_main ("tests" >::: tests)
