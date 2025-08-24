open OUnit2

(* Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists. *)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode l =
  let format temp h = if temp = 1 then One h else Many (temp, h) in
  let rec aux l curr temp output =
    match l, curr with
    | [], None -> []
    | [], Some h -> output @ [format temp h]
    | h :: t, None -> aux t (Some h) 1 output
    | h :: t, Some a when h <> a -> aux t (Some h) 1 (output @ [format temp a])
    | _ :: t, _ -> aux t curr (temp + 1) output in
  aux l None 0 []

(* Testing *)

let ae exp got _test_ctxt = assert_equal exp got
let tests = [
  "test1" >:: ae [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
]

let () =
  run_test_tt_main ("tests" >::: tests)
