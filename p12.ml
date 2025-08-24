open OUnit2

(* Given a run-length code list generated as specified in the previous problem, construct its uncompressed version. *)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode l =
  let rec aux l out =
    match l with
    | [] -> out
    | h :: t ->
      match h with
      | One x -> aux t (out @ [x])
      | Many (n,x) -> aux t (out @ List.init n (fun _ -> x)) in
  aux l []

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")])
]

let () =
  run_test_tt_main ("tests" >::: tests)
