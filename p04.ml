open OUnit2

(* Find the number of elements of a list. Bonus for a tail recursive solution. *)

let length l =
  let rec aux l curr =
    match l with
    | [] -> curr
    | _ :: t -> aux t (curr + 1) in
  aux l 0

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae 3 (length ["a"; "b"; "c"]);
  "test2" >:: ae 0 (length [])
]

let () =
  run_test_tt_main ("tests" >::: tests)
