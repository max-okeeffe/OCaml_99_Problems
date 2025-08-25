open OUnit2

(* Replicate the elements of a list a given number of times. *)

let replicate l n =
  let rec aux l out =
    match l with
    | [] -> out
    | h :: t -> aux t (out @ List.init n (fun _ -> h)) in
  aux l []

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] (replicate ["a"; "b"; "c"] 3)
]

let () =
  run_test_tt_main ("tests" >::: tests)
