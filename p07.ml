open OUnit2

(* Flatten a nested list structure. *)

type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten l =
  let rec aux l curr =
    match l with
    | [] -> curr
    | h :: t ->
      match h with
      | One x -> aux t (curr @ [x])
      | Many xs -> aux (xs @ t) curr in
  aux l []

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae ["a"; "b"; "c"; "d"; "e"] (flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]])
]

let () =
  run_test_tt_main ("tests" >::: tests)
