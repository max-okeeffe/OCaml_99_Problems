open OUnit2

(* Eliminate consecutive duplicates of list elements. *)

let compress l =
  let rec aux l curr output =
    match l, curr with
    | [], _ -> output
    | h :: t, None -> aux t (Some h) (output @ [h])
    | h :: t, Some a when h <> a -> aux t (Some h) (output @ [h])
    | _ :: t, _ -> aux t curr output in
  aux l None []

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae ["a"; "b"; "c"; "a"; "d"; "e"] (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
]

let () =
  run_test_tt_main ("tests" >::: tests)
