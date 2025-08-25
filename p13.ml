open OUnit2

(* Compress a list using run-length encoding *)

let encode l =
  let rec aux l curr temp output =
    match l, curr with
    | [], None -> []
    | [], Some h -> output @ [(temp, h)]
    | h :: t, None -> aux t (Some h) 1 output
    | h :: t, Some a when h <> a -> aux t (Some h) 1 (output @ [(temp, a)])
    | _ :: t, _ -> aux t curr (temp + 1) output in
  aux l None 0 []

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
]

let () =
  run_test_tt_main ("tests" >::: tests)
