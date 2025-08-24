open OUnit2

(* Find the last two (last and penultimate) elements of a list. *)

let rec last_two = function
  | [] -> None
  | [_] -> None
  | [x;y] -> Some (x,y)
  | _ :: t -> last_two t

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae (Some ("c", "d")) (last_two ["a"; "b"; "c"; "d"]);
  "test2" >:: ae None (last_two ["a"])
]

let () =
  run_test_tt_main ("tests" >::: tests)
